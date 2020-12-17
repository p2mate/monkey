use super::lexer::Token;

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub expression: Expression,
    token: Token,
}

#[derive(Debug, Clone)]
pub enum Expression {
    None,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
}

#[derive(Debug, Clone)]
pub struct Identifier {
    token: Token,
    pub value: String,
}

impl Identifier {
    pub fn from_token(t: Token) -> Option<Self> {
        if let Token::Ident(name) = t.clone() {
            Some(Identifier {
                token: t,
                value: name,
            })
        } else {
            None
        }
    }

    pub fn token_literal(&self) -> String {
        format!("{}", self.token)
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn from_token(t: Token) -> Option<Self> {
        if let Token::Int(number) = t.clone() {
            Some(IntegerLiteral {
                token: t,
                value: number,
            })
        } else {
            None
        }
    }

    pub fn token_literal(&self) -> String {
        format!("{}", self.token)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOperator {
    Minus,
    Bang,
}

impl PrefixOperator {
    fn from_token(t: Token) -> Self {
        match t {
            Token::Minus => Self::Minus,
            Token::Bang => Self::Bang,
            _ => unimplemented!(),
        }
    }
}

impl PrefixExpression {
    pub fn from_token(t: Token, expr: Option<Expression>) -> Option<Self> {
        match t {
            Token::Bang | Token::Minus => Some(PrefixExpression {
                token: t.clone(),
                operator: PrefixOperator::from_token(t.clone()),
                right: match expr {
                        Some(e) => Some(Box::new(e)),
                        None => None,
                }
            }),
            _ => None,
        }
    }
}
#[derive(Debug, Clone)]
pub struct PrefixExpression {
    token: Token,
    pub operator: PrefixOperator,
    pub right: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    token: Token,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    token: Token,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    Expression(Expression),
    Identifier(Identifier),
    ExpressionStatement(ExpressionStatement),
}

impl ASTNode {
    pub fn token_literal(&self) -> String {
        let token = match self.clone() {
            Self::LetStatement(l) => l.token,
            Self::Identifier(l) => l.token,
            Self::ReturnStatement(l) => l.token,
            _ => unreachable!(),
        };
        format!("{}", token)
    }

    pub fn new_let_node(token: Token, name: Identifier, expression: Expression) -> Self {
        let node = LetStatement {
            token,
            name,
            expression,
        };
        ASTNode::LetStatement(node)
    }

    pub fn new_return_node(token: Token, expression: Expression) -> Self {
        let node = ReturnStatement { token, expression };
        ASTNode::ReturnStatement(node)
    }

    pub fn new_expression_statement_node(token: Token, expression: Expression) -> Self {
        let node = ExpressionStatement { token, expression };
        ASTNode::ExpressionStatement(node)
    }

    pub fn is_same_type_as(&self, other: &ASTNode) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<ASTNode>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }

    pub fn push(&mut self, node: ASTNode) {
        self.statements.push(node);
    }
}
