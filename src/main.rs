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

use std::time::SystemTime;

use nomos::{Context, parser::parse};

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

fn main() {
    setup_logger().expect("Failed to initialize Logger");
    log::info!("Logger initialized");
    let mut ctx = Context {
        variables: Default::default(),
        sources: Default::default(),
    };
    let source = "<< test >>";
    let contents = "def test x: i32, y: i32 = match x + y with | 1 -> 2 | _ -> 3";

    ctx.intern_source(source, contents);

    match parse(&mut ctx, source) {
        Ok(ast) => {
            if ast.contains_error() {
                for e in ast.item.collect_errors() {
                    e.report().print(&mut ctx).unwrap();
                }
            } else {
                println!("{}", ast.s_expr());
            }
        }
        Err(e) => {
            e.report().print(ctx).unwrap();
        }
    }
}
