use std::time::SystemTime;

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
        .level(log::LevelFilter::Off)
        .chain(std::io::stdout())
        .chain(fern::log_file("output.log")?)
        .apply()?;
    Ok(())
}

fn main() {
    /*let file = "test.stream";
    let mut buf = Vec::new();
    File::open(file).unwrap().read_to_end(&mut buf).unwrap();
    let input = String::from_utf8(buf).unwrap();
    match Context::new(file, &input).lex() {
        Ok(lexed) => {
            match parse(lexed) {
                Ok(ast) => {
                    println!("{:?}", ast);
                }
                Err(errs) => {
                    for err in errs {
                        let cache = err.cache();
                        err.report().print(cache).unwrap();
                    }
                }
            };
        }
        Err(errs) => {
            for err in errs {
                let cache = err.cache();
                err.report().print(cache).unwrap();
            }
        }
    }*/
}
