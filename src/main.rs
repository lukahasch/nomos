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
    let contents = "fn(n) -> if [!] then n else \n!";

    ctx.intern_source(source, contents);

    match parse(&mut ctx, source) {
        Ok(ast) => {
            dbg!(ast);
        }
        Err(e) => {
            e.report().print(ctx).unwrap();
        }
    }
}
