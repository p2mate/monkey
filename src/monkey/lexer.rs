use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal(Option<char>),
    EOF,

    // Identifiers and literals
    Ident(String),
    Int(i64),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    EQ,
    NotEQ,

    // Delimiters
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

impl Token {
    pub fn is_same_type_as(&self, other: &Token) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}
#[derive(Clone)]
pub struct Lexer<'a> {
    input: String,
    ch: Option<char>,
    keywords: HashMap<&'a str, Token>,
}

impl<'a> Lexer<'a> {
    pub fn from_str<S: AsRef<str>>(input: S) -> Self {
        let keywords = [
            ("let", Token::Let),
            ("fn", Token::Function),
            ("if", Token::If),
            ("else", Token::Else),
            ("true", Token::True),
            ("false", Token::False),
            ("return", Token::Return),
        ]
        .iter()
        .cloned()
        .collect();
        let mut l = Lexer {
            input: input.as_ref().to_owned(),
            ch: None,
            keywords,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        self.ch = if self.input.len() > 0 {
            Some(self.input.remove(0))
        } else {
            None
        };
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(0)
    }

    fn read_identifier(&mut self) -> Token {
        let mut id = String::new();
        while let Some(x) = self.ch {
            if x.is_alphabetic() {
                id.push(x);
                self.read_char();
            } else {
                break;
            }
        }

        if let Some(t) = self.keywords.get(id.as_str()) {
            t.clone()
        } else {
            Token::Ident(id)
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(x) = self.ch {
            if x.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> Token {
        let mut number_str = String::new();
        while let Some(x) = self.ch {
            if x.is_numeric() {
                number_str.push(x);
                self.read_char();
            } else {
                break;
            }
        }

        let number = number_str.parse::<i64>();
        if let Ok(n) = number {
            Token::Int(n)
        } else {
            Token::Illegal(self.ch)
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.ch {
            None => {
                self.read_char();
                Token::EOF
            }
            Some('=') => {
                let r = match self.peek_char() {
                    Some('=') => { self.read_char(); Token::EQ },
                    _ => Token::Assign,
                };
                self.read_char();
                r
            }
            Some(';') => {
                self.read_char();
                Token::Semicolon
            }
            Some('(') => {
                self.read_char();
                Token::Lparen
            }
            Some(')') => {
                self.read_char();
                Token::Rparen
            }
            Some(',') => {
                self.read_char();
                Token::Comma
            }
            Some('+') => {
                self.read_char();
                Token::Plus
            }
            Some('-') => {
                self.read_char();
                Token::Minus
            }
            Some('{') => {
                self.read_char();
                Token::Lbrace
            }
            Some('}') => {
                self.read_char();
                Token::Rbrace
            }
            Some('!') => {
                let r = match self.peek_char() {
                    Some('=') => { self.read_char(); Token::NotEQ },
                    _ => Token::Bang,
                };
                self.read_char();
                r
            }
            Some('/') => {
                self.read_char();
                Token::Slash
            }
            Some('*') => {
                self.read_char();
                Token::Asterisk
            }
            Some('<') => {
                self.read_char();
                Token::LT
            }
            Some('>') => {
                self.read_char();
                Token::GT
            }
            Some(l) if l.is_alphabetic() => self.read_identifier(),
            Some(l) if l.is_numeric() => self.read_number(),
            Some(l) => {
                self.read_char();
                Token::Illegal(Some(l))
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::EOF => None,
            x => Some(x),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, dest: &mut std::fmt::Formatter) -> std::fmt::Result {    
        match self.clone() {
            Self::Illegal(_) => write!(dest, "illegal character"),
            Self::EOF => write!(dest,"eof"),
            Self::Ident(x) => write!(dest, "{}", x),
            Self::Int(x) => write!(dest, "{}", x),
            Self::Assign => write!(dest, "="),
            Self::Plus => write!(dest,"+"),
            Self::Minus => write!(dest,"-"),
            Self::Bang => write!(dest,"!"),
            Self::Asterisk => write!(dest,"*"),
            Self::Slash => write!(dest,"/"),
            Self::LT => write!(dest,"<"),
            Self::GT => write!(dest,">"),
            Self::EQ => write!(dest,"=="),
            Self::NotEQ => write!(dest,"!="),
            Self::Comma => write!(dest,","),
            Self::Semicolon => write!(dest,";"),
            Self::Lparen => write!(dest,"("),
            Self::Rparen => write!(dest,")"),
            Self::Lbrace => write!(dest,"{{"),
            Self::Rbrace => write!(dest,"}}"),
            Self::Function => write!(dest,"function"),
            Self::Let => write!(dest,"let"),
            Self::If => write!(dest,"if"),
            Self::Else => write!(dest,"else"),
            Self::Return => write!(dest,"return"),
            Self::True => write!(dest,"true"),
            Self::False => write!(dest,"false"),
        }  
    }
}
#[test]
fn test_next_token_0() {
    use Token::*;

    let input = "=+(){},;";
    let expected_token = [
        Assign, Plus, Lparen, Rparen, Lbrace, Rbrace, Comma, Semicolon,
    ];

    let mut lexer = Lexer::from_str(input).into_iter();
    for t in expected_token.iter() {
        assert!(lexer.next() == Some(t.clone()));
    }
}

#[test]
fn test_next_token_1() {
    use Token::*;

    let input = "let five = 5;
         let ten = 10;

         let add = fn(x, y) {
             x + y;
         };

         let result = add(five, ten);
         ";
    let expected_token = [
        Let,
        Ident("five".to_owned()),
        Assign,
        Int(5),
        Semicolon,
        Let,
        Ident("ten".to_owned()),
        Assign,
        Int(10),
        Semicolon,
        Let,
        Ident("add".to_owned()),
        Assign,
        Function,
        Lparen,
        Ident("x".to_owned()),
        Comma,
        Ident("y".to_owned()),
        Rparen,
        Lbrace,
        Ident("x".to_owned()),
        Plus,
        Ident("y".to_owned()),
        Semicolon,
        Rbrace,
        Semicolon,
        Let,
        Ident("result".to_owned()),
        Assign,
        Ident("add".to_owned()),
        Lparen,
        Ident("five".to_owned()),
        Comma,
        Ident("ten".to_owned()),
        Rparen,
        Semicolon,
    ];
    let mut lexer = Lexer::from_str(input).into_iter();
    for t in expected_token.iter() {
        assert!(lexer.next() == Some(t.clone()));
    }
}

#[test]
fn test_next_token_2() {
    use Token::*;

    let input = "let five = 5;
         let ten = 10;

         let add = fn(x, y) {
             x + y;
         };

         let result = add(five, ten);
         !-/*5;
         5 < 10 > 5;

         if (5 < 10) {
            return true;
        } else {
            return false;
        }
    
        10 == 10;
        10 != 9;
        ";
    let expected_token = [
        Let,
        Ident("five".to_owned()),
        Assign,
        Int(5),
        Semicolon,
        Let,
        Ident("ten".to_owned()),
        Assign,
        Int(10),
        Semicolon,
        Let,
        Ident("add".to_owned()),
        Assign,
        Function,
        Lparen,
        Ident("x".to_owned()),
        Comma,
        Ident("y".to_owned()),
        Rparen,
        Lbrace,
        Ident("x".to_owned()),
        Plus,
        Ident("y".to_owned()),
        Semicolon,
        Rbrace,
        Semicolon,
        Let,
        Ident("result".to_owned()),
        Assign,
        Ident("add".to_owned()),
        Lparen,
        Ident("five".to_owned()),
        Comma,
        Ident("ten".to_owned()),
        Rparen,
        Semicolon,
        Bang,
        Minus,
        Slash,
        Asterisk,
        Int(5),
        Semicolon,
        Int(5),
        LT,
        Int(10),
        GT,
        Int(5),
        Semicolon,
        If,
        Lparen,
        Int(5),
        LT,
        Int(10),
        Rparen,
        Lbrace,
        Return,
        True,
        Semicolon,
        Rbrace,
        Else,
        Lbrace,
        Return,
        False,
        Semicolon,
        Rbrace,
        Int(10),
        EQ,
        Int(10),
        Semicolon,
        Int(10),
        NotEQ,
        Int(9),
        Semicolon,
    ];
    let mut lexer = Lexer::from_str(input).into_iter();
    for t in expected_token.iter() {
        assert!(lexer.next() == Some(t.clone()));
    }
}
