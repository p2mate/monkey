mod monkey;
use monkey::repl;

fn main() -> std::io::Result<()> {
    let mut reader = std::io::BufReader::new(std::io::stdin());
    repl::Repl::start(&mut reader, &mut std::io::stdout().lock())?;
    Ok(())
}