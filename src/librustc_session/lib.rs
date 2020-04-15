#![feature(crate_visibility_modifier)]

pub mod cgu_reuse_tracker;
pub mod utils;
pub use rustc_lint_types as lint;
pub use rustc_lint_types::{declare_lint, declare_lint_pass, declare_tool_lint, impl_lint_pass};
pub mod parse;

mod code_stats;
#[macro_use]
pub mod config;
pub mod filesearch;
mod options;
pub mod search_paths;

mod session;
pub use session::*;

pub mod output;

pub use getopts;
