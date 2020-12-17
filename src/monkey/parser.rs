use super::ast::{self, ASTNode, Program};
use super::lexer::{Lexer, Token};

struct Parser<'a> {
    lexer: &'a Lexer<'a>,
    errors: Vec<ParseError>,
}

#[derive(Debug, Clone)]
struct ParseError {
    msg: String,
}

enum ExpressionPrio {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

impl ParseError {
    fn from_expected(got: Token, expected: Token) -> Self {
        let msg = format!("expected token {}, got {} instead", expected, got);
        ParseError { msg }
    }
    fn from_string(msg: &str) -> Self {
        ParseError {
            msg: String::from(msg),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, dest: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(dest, "{}", self.msg)
    }
}

impl<'a> Parser<'a> {
    fn new(l: &'a mut Lexer<'a>) -> Self {
        Parser {
            lexer: l,
            errors: Vec::new(),
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();
        let lexer = self.lexer.clone();
        let mut pk = lexer.peekable();
        while let Some(token) = pk.next() {
            match self.parse_statement(&mut pk, token) {
                Ok(s) => program.push(s),
                Err(e) => self.errors.push(e),
            }
        }
        program
    }

    fn parse_statement<I>(
        &mut self,
        pk: &mut std::iter::Peekable<I>,
        token: Token,
    ) -> Result<ASTNode, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        match token {
            Token::Let => self.parse_let_statement(pk),
            Token::Return => self.parse_return_statement(pk),
            _ => self.parse_expression_statement(pk, token),
        }
    }

    fn parse_let_statement<I>(
        &mut self,
        pk: &mut std::iter::Peekable<I>,
    ) -> Result<ASTNode, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        let ident_token = self.expect_peek(&Token::Ident(String::from("")), pk)?;
        let name = ast::Identifier::from_token(ident_token.clone()).unwrap();

        let _ = self.expect_peek(&Token::Assign, pk)?;

        while let Some(t) = pk.next() {
            if t == Token::Semicolon {
                break;
            }
        }
        Ok(ASTNode::new_let_node(
            Token::Let,
            name,
            ast::Expression::None,
        ))
    }

    fn parse_return_statement<I>(
        &mut self,
        pk: &mut std::iter::Peekable<I>,
    ) -> Result<ASTNode, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        pk.next();

        while let Some(t) = pk.next() {
            if t == Token::Semicolon {
                break;
            }
        }
        Ok(ASTNode::new_return_node(
            Token::Return,
            ast::Expression::None,
        ))
    }

    fn parse_expression_statement<I>(
        &mut self,
        pk: &mut std::iter::Peekable<I>,
        token: Token,
    ) -> Result<ASTNode, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        // let token = pk.peek().ok_or(ParseError::from_string("Unexpected EOF"))?.clone();
        let expression = self
            .parse_expression(pk, token.clone(), ExpressionPrio::Lowest)
            .ok_or(ParseError::from_string("Unable to parse expression"))?;
        while let Some(t) = pk.next() {
            if t == Token::Semicolon {
                break;
            }
        }
        Ok(ASTNode::new_expression_statement_node(token, expression))
    }

    fn parse_expression<I>(
        &mut self,
        pk: &mut std::iter::Peekable<I>,
        token: Token,
        prio: ExpressionPrio,
    ) -> Option<ast::Expression>
    where
        I: Iterator<Item = Token>,
    {
        use ast::*;
        /* Prefix tokens */
        match token.clone() {
            Token::Ident(_) => Some(Expression::Identifier(
                Identifier::from_token(token).unwrap(),
            )),
            Token::Int(_) => Some(Expression::IntegerLiteral(
                IntegerLiteral::from_token(token).unwrap(),
            )),
            Token::Bang | Token::Minus => match pk.next() {
                Some(x) => Some(Expression::PrefixExpression(
                    PrefixExpression::from_token(
                        token,
                        self.parse_expression(pk, x, ExpressionPrio::Prefix),
                    )
                    .unwrap(),
                )),
                None => None,
            },

            _ => {
                self.errors.push(ParseError::from_string(
                    &format!("Don't know how to parse prefix {}", token).to_string(),
                ));
                None
            }
        }
    }

    fn expect_peek<I>(
        &mut self,
        token: &Token,
        pk: &mut std::iter::Peekable<I>,
    ) -> Result<Token, ParseError>
    where
        I: Iterator<Item = Token>,
    {
        if let Some(t) = pk.peek() {
            if t.is_same_type_as(token) {
                Ok(pk.next().unwrap())
            } else {
                Err(ParseError::from_expected(t.clone(), token.clone()))
            }
        } else {
            Err(ParseError::from_string("Unexpected EOF"))
        }
    }
}

#[test]
fn test_let_statements() {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let mut l = Lexer::from_str(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();
    assert!(p.errors.len() == 0);

    assert!(program.statements.len() == 3);

    let tests = ["x", "y", "foobar"];
    for (i, identifier) in tests.iter().enumerate() {
        let statement = program.statements[i].clone();
        assert!(statement.token_literal() == "let");
        if let ASTNode::LetStatement(ls) = statement {
            assert!(ls.name.value.as_str() == *identifier);
            assert!(ls.name.token_literal().as_str() == *identifier);
        } else {
            assert!(false);
        }
    }
}

#[test]
fn test_return_statements() {
    let input = "
        return 5;
        return 10;
        return 993322;
    ";

    let mut l = Lexer::from_str(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();
    assert!(p.errors.len() == 0);

    assert!(program.statements.len() == 3);
    for s in program.statements {
        assert!(s.is_same_type_as(&ASTNode::new_return_node(
            Token::Return,
            ast::Expression::None
        )));
        assert!(s.token_literal() == "return");
    }
}

#[test]
fn test_identifiers() {
    let input = "foobar;";

    let mut l = Lexer::from_str(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();
    assert!(p.errors.len() == 0);

    assert!(program.statements.len() == 1);
    match program.statements[0].clone() {
        ast::ASTNode::ExpressionStatement(expression) => match expression.expression {
            ast::Expression::Identifier(identifier) => {
                assert!(identifier.value == "foobar");
                assert!(identifier.token_literal() == "foobar")
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

#[test]
fn test_integer_literals() {
    let input = "5;";

    let mut l = Lexer::from_str(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();
    assert!(p.errors.len() == 0);

    assert!(program.statements.len() == 1);
    match program.statements[0].clone() {
        ast::ASTNode::ExpressionStatement(expression) => match expression.expression {
            ast::Expression::IntegerLiteral(x) => {
                assert!(x.value == 5);
                assert!(x.token_literal() == "5")
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

#[test]
fn test_parsing_prefix_expr() {
    use ast::PrefixOperator;

    struct Test {
        input: String,
        operator: PrefixOperator,
        value: i64,
    }
    let test_input = [
        Test {
            input: "!5;".to_string(),
            operator: PrefixOperator::Bang,
            value: 5,
        },
        Test {
            input: "-15;".to_string(),
            operator: PrefixOperator::Minus,
            value: 15,
        },
    ];

    for input in test_input.iter() {
        let mut l = Lexer::from_str(input.input.clone());
        let mut p = Parser::new(&mut l);

        let program = p.parse_program();
        dbg!(&program);
        assert!(p.errors.len() == 0);
        assert!(program.statements.len() == 1);

        match program.statements[0].clone() {
            ast::ASTNode::ExpressionStatement(expression) => match expression.expression {
                ast::Expression::PrefixExpression(x) => {
                    assert!(x.operator == input.operator);
                    match x.right {
                        Some(y) => match *y {
                            ast::Expression::IntegerLiteral(z) => assert!(z.value == input.value),
                            _ => unreachable!(),
                        },
                        None => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
