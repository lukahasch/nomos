#![feature(
    type_alias_impl_trait,
    const_ops,
    unboxed_closures,
    try_blocks,
    try_trait_v2,
    never_type,
    min_specialization,
    macro_metavar_expr,
    if_let_guard
)]

use std::{ops::Deref, time::SystemTime};

use nomos::Context;
use reedline::{FileBackedHistory, Prompt, PromptEditMode, PromptHistorySearch, Reedline, Signal};

fn setup_logger() -> Result<(), fern::InitError> {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{} {} {}] {}",
                humantime::format_rfc3339_seconds(SystemTime::now()),
                record.level(),
                record.target(),
                message
            ))
        })
        .level(log::LevelFilter::Debug)
        .chain(std::io::stdout())
        .chain(fern::log_file("output.log")?)
        .apply()?;
    Ok(())
}

struct ReplPrompt {
    index: usize,
}

impl Prompt for ReplPrompt {
    fn render_prompt_left(&self) -> std::borrow::Cow<'_, str> {
        format!("repl@{} > ", self.index).into()
    }

    fn render_prompt_right(&self) -> std::borrow::Cow<'_, str> {
        "".into()
    }

    // Optional: how the prompt looks during editing/searching
    fn render_prompt_indicator(&self, _mode: PromptEditMode) -> std::borrow::Cow<'_, str> {
        "".into()
    }

    fn render_prompt_multiline_indicator(&self) -> std::borrow::Cow<'_, str> {
        "... ".into()
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: PromptHistorySearch,
    ) -> std::borrow::Cow<'_, str> {
        ": ".into()
    }
}

fn main() {
    setup_logger().expect("Failed to initialize Logger");
    log::info!("PROGRAM START");

    let mut ctx = Context::new();

    let mut line_editor = Reedline::create().with_history(Box::new(
        FileBackedHistory::with_file(100, "repl_history.txt".into())
            .expect("Failed to create history file"),
    ));
    for index in 0.. {
        let source = Box::leak(Box::new(format!("<< repl@{index} >>")));
        let contents = match line_editor.read_line(&ReplPrompt { index }) {
            Ok(Signal::Success(line)) => line,
            Ok(Signal::CtrlC) | Ok(Signal::CtrlD) => {
                println!("Exiting REPL.");
                break;
            }
            Err(err) => {
                eprintln!("Error reading line: {err}");
                break;
            }
        };

        ctx.intern_source(source, &contents);

        match ctx.store(source) {
            Ok(ast) => {
                ctx.egraph.rebuild();
                println!(
                    "rebuilt: {}",
                    ctx.show(ctx.egraph.extract(ast, ()).unwrap().deref())
                )
            }
            Err(e) => {
                for e in e {
                    e.report().print(&mut ctx).unwrap();
                }
            }
        }
    }
}
