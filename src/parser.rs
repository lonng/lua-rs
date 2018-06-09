use ::{Error, NO_JUMP, Result};
use ast::*;
use scanner::{Scanner, Token};
use state::State;
use std::io::{BufReader, Read};
use std::mem;
use vm::Chunk;

static BINARY_PRIORITY: &'static [(u8, u8)/*(left, right)*/; 15] = &[
    (6, 6), (6, 6), (7, 7), (7, 7), (7, 7), // `+' `-' `*' `/' `%'
    (10, 9), (5, 4), // ^, .. (right associative)
    (3, 3), (3, 3), (3, 3), // ==, <, <=
    (3, 3), (3, 3), (3, 3), // ~=, >, >=
    (2, 2), (1, 1), // and, or
];

const UNARY_PRIORITY: u8 = 8;


pub fn unary_op(t: &Token) -> UnaryOpr {
    match *t {
        Token::Not => UnaryOpr::Not,
        Token::Char(c) if c == '-' => UnaryOpr::Minus,
        Token::Char(c) if c == '#' => UnaryOpr::Length,
        _ => UnaryOpr::NoUnary
    }
}

pub fn binary_op(t: &Token) -> BinaryOpr {
    match *t {
        Token::Char(c) if c == '+' => BinaryOpr::Add,
        Token::Char(c) if c == '-' => BinaryOpr::Sub,
        Token::Char(c) if c == '*' => BinaryOpr::Mul,
        Token::Char(c) if c == '/' => BinaryOpr::Div,
        Token::Char(c) if c == '%' => BinaryOpr::Mod,
        Token::Char(c) if c == '^' => BinaryOpr::Pow,
        Token::Concat => BinaryOpr::Concat,
        Token::NE => BinaryOpr::NE,
        Token::Eq => BinaryOpr::Eq,
        Token::Char(c) if c == '<' => BinaryOpr::LT,
        Token::LE => BinaryOpr::LE,
        Token::Char(c) if c == '>' => BinaryOpr::GT,
        Token::GE => BinaryOpr::GE,
        Token::And => BinaryOpr::And,
        Token::Or => BinaryOpr::Or,
        _ => BinaryOpr::NoBinary,
    }
}

#[derive(Debug)]
pub struct Parser<R> {
    source: String,
    scanner: Scanner<R>,
    line_number: i32,
    token: Token,
    ahead_token: Token,
}

impl<R: Read> Parser<R> {
    pub fn new(reader: BufReader<R>, name: String) -> Parser<R> {
        Parser {
            source: name,
            scanner: Scanner::new(reader),
            line_number: 0,
            token: Token::EOF,
            ahead_token: Token::EOF,
        }
    }

    pub fn parse(&mut self) -> Result<Box<Chunk>> {
        self.next()?;
        let stmts = self.block()?;
        println!("Stmts => {:#?}", stmts);
        self.check(Token::EOF)?;
        Ok(Chunk::new())
    }

    fn next(&mut self) -> Result<()> {
        self.line_number = self.scanner.line_number();
        self.token = match self.ahead_token {
            Token::EOF => self.scanner.scan()?,
            _ => {
                let mut ahead = Token::EOF;
                mem::swap(&mut ahead, &mut self.ahead_token);
                ahead
            }
        };
        println!("token:{} => {:?}", self.line_number, self.token);
        Ok(())
    }

    fn look_ahead(&mut self) -> Result<()> {
        debug_assert!(&self.ahead_token == &Token::EOF);
        self.ahead_token = self.scanner.scan()?;
        Ok(())
    }

    fn block_follow(&self, with_until: bool) -> bool {
        match self.token {
            Token::Else | Token::Elseif | Token::End | Token::EOF => true,
            Token::Until => with_until,
            _ => false,
        }
    }

    fn block(&mut self) -> Result<Vec<StmtNode>> {
        let mut stmts: Vec<StmtNode> = vec![];
        loop {
            if self.block_follow(true) {
                break;
            }

            stmts.push(self.statement()?);
            if self.token == Token::Return {
                break;
            }
        }
        return Ok(stmts);
    }

    fn testnext(&mut self, t: &Token) -> Result<bool> {
        if self.token == *t {
            self.next()?;
            return Ok(true);
        }
        Ok(false)
    }

    fn statement(&mut self) -> Result<StmtNode> {
        loop {
            match self.token {
                Token::Char(char) if char == ';' => self.next()?,
                _ => break
            }
        }
        let line = self.line_number;
        // is not last statement
        let stmt = match self.token {
            Token::If => self.ifstat(line)?,
            Token::While => self.whilestat(line)?,
            Token::Do => {
                self.next()?;
                let stmts = self.block()?;
                self.check_match(Token::End, Token::Do, line)?;
                StmtNode::new(Stmt::DoBlock(stmts))
            }
            Token::For => self.forstat(line)?,
            Token::Repeat => self.repeatstat(line)?,
            Token::Function => self.funcstat(line)?,
            Token::Local => {
                self.next()?;
                if self.testnext(&Token::Function)? {
                    self.localfunc()?
                } else {
                    self.localstat()?
                }
            }
            Token::Return => {
                self.next()?;
                self.retstat()?
            }
            Token::Break => self.breakstat()?,
            _ => self.exprstat()?
        };
        Ok(stmt)
    }

    fn field(&mut self) -> Result<Field> {
        let field = match self.token {
            Token::Char('[') => {
                self.next()?;
                let key = self.expression()?;
                self.check_next(Token::Char(']'))?;
                self.check_next(Token::Char('='))?;
                let val = self.expression()?;
                Field::new(Some(key), val)
            }
            Token::Ident(_) => {
                let key = self.expression()?;
                self.check_next(Token::Char('='))?;
                let val = self.expression()?;
                Field::new(Some(key), val)
            }
            _ => {
                let val = self.expression()?;
                Field::new(None, val)
            }
        };
        Ok(field)
    }

    fn table_ctor(&mut self) -> Result<ExprNode> {
        debug_assert!(self.token == Token::Char('{'));
        let mut fields: Vec<Field> = vec![];
        self.next()?;
        while self.token != Token::Char('}') {
            let field = self.field()?;
            fields.push(field);
            match self.token {
                Token::Char(',') | Token::Char(';') => self.next()?,
                _ => {}
            }
        }
        let line = self.line_number;
        self.check_next(Token::Char('}'))?;
        Ok(ExprNode::new(Expr::Table(fields)))
    }

    /// ```BNF
    /// namelist :: Ident | namelist ',' Ident
    /// ```
    fn namelist(&mut self) -> Result<Vec<String>> {
        let mut names: Vec<String> = vec![];
        loop {
            match self.token {
                Token::Ident(ref name) => names.push(name.clone()),
                Token::Char(',') => self.unexpected(Token::Char(','))?,
                _ => break
            }
            self.next()?;
            self.testnext(&Token::Char(','))?;
        }
        Ok(names)
    }

    fn function(&mut self) -> Result<ExprNode> {
        debug_assert!(self.token == Token::Function);
        self.next()?;

        // paramater list
        self.check_next(Token::Char('('))?;
        let mut parlist = ParList::new();
        if let Token::Ident(_) = self.token {
            let names = self.namelist()?;
            parlist.set_names(names);
        }
        if Token::Dots == self.token {
            parlist.set_vargs(true);
            self.next()?;
        }
        self.check_next(Token::Char(')'))?;

        let block = self.block()?;
        let line = self.line_number;
        self.check_next(Token::End)?;
        Ok(ExprNode::new(Expr::Function(parlist, block)))
    }

    /// Prefix expression
    ///
    /// ```BNF
    /// prefixexp -> NAME | '(' expr ')'
    /// ```
    fn prefixexp(&mut self) -> Result<ExprNode> {
        let expr = match self.token {
            Token::Ident(ref s) => {
                let expr = ExprNode::new(Expr::Ident(s.clone()));
                expr
            }
            Token::Char('(') => {
                self.next()?;
                let line = self.line_number;
                let expr = self.expression()?;
                self.check(Token::Char(')'))?;
                expr
            }
            _ => return Err(Error::SyntaxError("unexpected symbol".to_string()))
        };
        self.next()?;
        Ok(expr)
    }

    /// Primary expression
    ///
    /// ```BNF
    /// primaryexp ->
    /// prefixexp { '.' NAME | '[' exp `]' | ':' NAME funcargs | funcargs }
    /// ```
    fn primaryexp(&mut self) -> Result<ExprNode> {
        let mut expr = self.prefixexp()?;
        loop {
            match self.token {
                Token::Char('.') => {
                    self.next()?;
                    if let Token::Ident(ref s) = self.token {
                        let key = ExprNode::new(Expr::String(s.clone()));
                        let mut get = Expr::AttrGet(Box::new(expr), Box::new(key));
                        expr = ExprNode::new(get);
                    } else {
                        return Err(Error::SyntaxError(format!("ident expected")));
                    }
                    self.next()?;
                }
                Token::Char('[') => unimplemented!(),
                Token::Char(':') => unimplemented!(),
                Token::Char('(') | Token::String(_) | Token::Char('{') => unimplemented!(),
                _ => return Ok(expr)
            };
        }
        unimplemented!()
    }

    /// Simple expression
    ///
    /// ```BNF
    /// simpleexp -> NUMBER | STRING | NIL | true | false | ... |
    /// constructor | FUNCTION body | primaryexp
    /// ```
    fn simple_expr(&mut self) -> Result<ExprNode> {
        let line = self.line_number;
        let mut node = match self.token {
            Token::True => ExprNode::new(Expr::True),
            Token::False => ExprNode::new(Expr::False),
            Token::Nil => ExprNode::new(Expr::Nil),
            Token::Number(n) => ExprNode::new(Expr::Number(n)),
            Token::Dots => ExprNode::new(Expr::Dots),
            Token::String(ref s) => ExprNode::new(Expr::String(s.clone())),
            Token::Char(c) if c == '{' => return Ok(self.table_ctor()?),
            Token::Function => return Ok(self.function()?),
            _ => return Ok(self.primaryexp()?)
        };
        self.next()?;
        //println!("simple_expr => {:#?}", node);
        Ok(node)
    }

    fn sub_expression(&mut self, limit: u8) -> Result<(ExprNode, BinaryOpr)> {
        let op = unary_op(&self.token);
        let line = self.line_number;
        let mut expr = if op != UnaryOpr::NoUnary {
            self.next()?;
            let sub = self.sub_expression(UNARY_PRIORITY)?.0;
            let mut node = ExprNode::new(Expr::UnaryOp(op, Box::new(sub)));
            node
        } else {
            self.simple_expr()?
        };
        expr.set_line(line);
        expr.set_last_line(self.line_number);

        let mut op = binary_op(&self.token);
        loop {
            if op == BinaryOpr::NoBinary || BINARY_PRIORITY[op as usize].1 <= limit {
                break;
            }
            self.next()?;
            let line = self.line_number;
            let sub = self.sub_expression(BINARY_PRIORITY[op as usize].0)?;
            let mut node = ExprNode::new(Expr::BinaryOp(op, Box::new(expr), Box::new(sub.0)));
            node.set_line(line);
            node.set_last_line(self.line_number);

            expr = node;
            op = sub.1;
        }

        Ok((expr, op))
    }

    fn expression(&mut self) -> Result<ExprNode> { Ok(self.sub_expression(0)?.0) }

    fn test_then_block(&mut self) -> Result<IfThenElse> {
        let mut jump_false = 0;
        self.next()?;
        let mut condition = self.expression()?;
        self.check_next(Token::Then)?;
        let then = if self.token == Token::Break {
            vec![StmtNode::new(Stmt::Break)]
        } else {
            self.block()?
        };
        Ok(IfThenElse::new(condition, then, vec![]))
    }

    /// ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
    fn ifstat(&mut self, line: i32) -> Result<StmtNode> {
        debug_assert!(self.token == Token::If);
        let mut ifthen = self.test_then_block()?;
        let mut elseifs: Vec<IfThenElse> = vec![];
        while self.token == Token::Elseif {
            let mut elseif = self.test_then_block()?;
            elseifs.push(elseif);
        }

        let mut els = if self.testnext(&Token::Else)? {
            self.block()?
        } else {
            vec![]
        };

        // link all elseif
        for mut elseif in elseifs.into_iter().rev().into_iter() {
            let mut e: Vec<Node<Stmt>> = vec![];
            mem::swap(&mut e, &mut els);
            elseif.set_els(e);
            els.push(StmtNode::new(Stmt::If(elseif)));
        }
        self.check_match(Token::End, Token::If, line)?;
        ifthen.set_els(els);
        Ok(StmtNode::new(Stmt::If(ifthen)))
    }

    fn whilestat(&mut self, line: i32) -> Result<StmtNode> { unimplemented!() }
    fn forstat(&mut self, line: i32) -> Result<StmtNode> { unimplemented!() }
    fn repeatstat(&mut self, line: i32) -> Result<StmtNode> { unimplemented!() }
    fn funcstat(&mut self, line: i32) -> Result<StmtNode> { unimplemented!() }
    fn localfunc(&mut self) -> Result<StmtNode> { unimplemented!() }

    fn localstat(&mut self) -> Result<StmtNode> {
        unimplemented!()
    }
    fn exprstat(&mut self) -> Result<StmtNode> { unimplemented!() }
    fn lebalstat(&mut self) -> Result<StmtNode> { unimplemented!() }
    fn breakstat(&mut self) -> Result<StmtNode> {
        unimplemented!();
    }

    fn retstat(&mut self) -> Result<StmtNode> {
        unimplemented!();
    }

    fn check_next(&mut self, expect: Token) -> Result<()> {
        self.check(expect)?;
        self.next()
    }

    fn check(&self, expect: Token) -> Result<()> {
        if self.token != expect {
            return Err(Error::SyntaxError(format!("{}:{}: {} expected",
                                                  self.source, self.line_number, expect.to_string())));
        }
        Ok(())
    }

    fn check_match(&mut self, what: Token, who: Token, line: i32) -> Result<()> {
        if !self.testnext(&what)? {
            let err = if line == self.line_number {
                format!("{}:{}: {} expected", self.source, self.line_number, what.to_string())
            } else {
                format!("{}:{}: {} expected (to close {} at line {}", self.source, self.line_number,
                        what.to_string(), who.to_string(), line)
            };
            Err(Error::SyntaxError(err))
        } else {
            Ok(())
        }
    }

    fn unexpected(&mut self, t: Token) -> Result<()> {
        let err = format!("{}:{}: unexpected: {}", self.source, self.line_number, t.to_string());
        Err(Error::SyntaxError(err))
    }
}