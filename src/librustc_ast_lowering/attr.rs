//! Attribute related logic in lowering.

use rustc_ast::{ast, attr};
use rustc_errors::{pluralize, struct_span_err, Applicability, DiagnosticBuilder};
use rustc_session::lint::{Level, LintDirective};
use rustc_session::parse::feature_err;
use rustc_session::Session;
use rustc_span::{sym, Span, Symbol};

fn bad_attr(sess: &Session, span: Span) -> DiagnosticBuilder<'_> {
    struct_span_err!(sess, span, E0452, "malformed lint attribute input")
}

fn extract_lint_reason(
    sess: &Session,
    metas: &[ast::NestedMetaItem],
    found: &mut bool,
) -> Option<Symbol> {
    match metas.last()?.meta_item()? {
        // Actual lint names handled later.
        ast::MetaItem { kind: ast::MetaItemKind::Word, .. } => {}
        ast::MetaItem { kind: ast::MetaItemKind::NameValue(name_value), path, span }
            if path == &sym::reason =>
        {
            *found = true;
            // FIXME (#55112): issue unused-attributes lint if we thereby
            // don't have any lint names (`#[level(reason = "foo")]`)
            if let ast::LitKind::Str(rationale, _) = name_value.kind {
                if !sess.features_untracked().lint_reasons {
                    feature_err(
                        &sess.parse_sess,
                        sym::lint_reasons,
                        *span,
                        "lint reasons are experimental",
                    )
                    .emit();
                }
                return Some(rationale);
            } else {
                bad_attr(sess, name_value.span)
                    .span_label(name_value.span, "reason must be a string literal")
                    .emit();
            }
        }
        ast::MetaItem { span, .. } => {
            bad_attr(sess, *span).span_label(*span, "bad attribute argument").emit();
        }
    }
    None
}

fn error_non_word_lint_attr(sess: &Session, li: &ast::NestedMetaItem) {
    let sp = li.span();
    let mut err = bad_attr(sess, sp);
    let mut add_label = true;
    if let Some(item) = li.meta_item() {
        if let ast::MetaItemKind::NameValue(_) = item.kind {
            if item.path == sym::reason {
                err.span_label(sp, "reason in lint attribute must come last");
                add_label = false;
            }
        }
    }
    if add_label {
        err.span_label(sp, "bad attribute argument");
    }
    err.emit();
}

fn extract_tool_name(sess: &Session, meta_item: &ast::MetaItem) -> Option<Option<Symbol>> {
    match &*meta_item.path.segments {
        [ast::PathSegment { ident, .. }, ..] if attr::is_known_lint_tool(*ident) => {
            Some(Some(ident.name))
        }
        [ast::PathSegment { ident, .. }, ..] => {
            struct_span_err!(
                sess,
                ident.span,
                E0710,
                "an unknown tool name found in scoped lint{}",
                sess.source_map()
                    .span_to_snippet(meta_item.path.span)
                    .map(|s| format!(": `{}`", s))
                    .unwrap_or_default(),
            )
            .emit();
            None
        }
        [] => Some(None),
    }
}

/// Lazily extract a list of `LintDirective`s from `attrs`,
/// performing a validation as we go,
/// including validating the attributes and marking them as used.
pub fn extract_lint_directives<'a>(
    sess: &'a Session,
    attrs: &'a [ast::Attribute],
) -> impl 'a + Iterator<Item = LintDirective> {
    attrs
        .iter()
        .filter_map(move |attr| {
            let level = Level::from_symbol(attr.name_or_empty())?;
            let meta = attr.meta()?;
            attr::mark_used(attr);
            let mut list = match meta.kind {
                // FIXME (#55112): issue unused-attributes lint for `#[level()]`
                ast::MetaItemKind::List(list) if !list.is_empty() => list,
                _ => return None,
            };
            // Before processing the lint names,
            // look for a reason (RFC 2383) at the end.
            let mut found_reason = false;
            let reason = extract_lint_reason(sess, &list, &mut found_reason);
            if found_reason {
                // Found reason, remove it from end of meta list.
                list.pop();
            }
            Some((list, level, reason))
        })
        .flat_map(move |(list, level, reason)| {
            list.into_iter().filter_map(move |li| {
                let meta_item = match li.meta_item() {
                    Some(meta_item) if meta_item.is_word() => meta_item,
                    _ => {
                        error_non_word_lint_attr(sess, &li);
                        return None;
                    }
                };
                let tool_name = extract_tool_name(sess, meta_item)?;
                let name = meta_item.path.segments.last().expect("empty lint name").ident.name;
                Some(LintDirective { level, name, tool_name, reason, span: li.span() })
            })
        })
}

pub fn add_elided_lifetime_in_path_suggestion(
    sess: &crate::Session,
    db: &mut DiagnosticBuilder<'_>,
    n: usize,
    path_span: Span,
    incl_angl_brckt: bool,
    insertion_span: Span,
    anon_lts: String,
) {
    let (replace_span, suggestion) = if incl_angl_brckt {
        (insertion_span, anon_lts)
    } else {
        // When possible, prefer a suggestion that replaces the whole
        // `Path<T>` expression with `Path<'_, T>`, rather than inserting `'_, `
        // at a point (which makes for an ugly/confusing label)
        if let Ok(snippet) = sess.source_map().span_to_snippet(path_span) {
            // But our spans can get out of whack due to macros; if the place we think
            // we want to insert `'_` isn't even within the path expression's span, we
            // should bail out of making any suggestion rather than panicking on a
            // subtract-with-overflow or string-slice-out-out-bounds (!)
            // FIXME: can we do better?
            if insertion_span.lo().0 < path_span.lo().0 {
                return;
            }
            let insertion_index = (insertion_span.lo().0 - path_span.lo().0) as usize;
            if insertion_index > snippet.len() {
                return;
            }
            let (before, after) = snippet.split_at(insertion_index);
            (path_span, format!("{}{}{}", before, anon_lts, after))
        } else {
            (insertion_span, anon_lts)
        }
    };
    db.span_suggestion(
        replace_span,
        &format!("indicate the anonymous lifetime{}", pluralize!(n)),
        suggestion,
        Applicability::MachineApplicable,
    );
}
