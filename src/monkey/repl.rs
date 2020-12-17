use std::io;
use super::lexer::Lexer;

pub struct Repl;

impl Repl {
    pub fn start<R: io::BufRead, W: io::Write>(input: &mut R, output: &mut W) -> io::Result<()> {
        loop {
            write!(output, ">>>")?;
            output.flush()?;
            let mut line = String::new();
            match input.read_line(&mut line) {
                Ok(0) => { write!(output, "\n")?; return Ok(()) },
                Err(e) => return Err(e),
                Ok(_) => {
                    let l = Lexer::from_str(line);
                    for t in l.into_iter() {
                        write!(output,"{:?}\n", t)?;
                    }                    
                }
            }    
        }
    }
}

