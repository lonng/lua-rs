use ::{Error, NO_JUMP, Result};
use ast::*;
use scanner::{Scanner, Token};
use state::State;
use std::io::{BufReader, Read};
use std::mem;

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
    line_number: u32,
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


    /// Here is the complete syntax of Lua in extended BNF. (It does not describe operator precedences.)
    ///
    /// ```BNF
    ///	chunk ::= {stat [`;´]} [laststat [`;´]]
    ///
    ///	block ::= chunk
    ///
    ///	stat ::=  varlist `=´ explist |
    ///		 functioncall |
    ///		 do block end |
    ///		 while exp do block end |
    ///		 repeat block until exp |
    ///		 if exp then block {elseif exp then block} [else block] end |
    ///		 for Name `=´ exp `,´ exp [`,´ exp] do block end |
    ///		 for namelist in explist do block end |
    ///		 function funcname funcbody |
    ///		 local function Name funcbody |
    ///		 local namelist [`=´ explist]
    ///
    ///	laststat ::= return [explist] | break
    ///
    ///	funcname ::= Name {`.´ Name} [`:´ Name]
    ///
    ///	varlist ::= var {`,´ var}
    ///
    ///	var ::=  Name | prefixexp `[´ exp `]´ | prefixexp `.´ Name
    ///
    ///	namelist ::= Name {`,´ Name}
    ///
    ///	explist ::= {exp `,´} exp
    ///
    ///	exp ::=  nil | false | true | Number | String | `...´ | function |
    ///		 prefixexp | tableconstructor | exp binop exp | unop exp
    ///
    ///	prefixexp ::= var | functioncall | `(´ exp `)´
    ///
    ///	functioncall ::=  prefixexp args | prefixexp `:´ Name args
    ///
    ///	args ::=  `(´ [explist] `)´ | tableconstructor | String
    ///
    ///	function ::= function funcbody
    ///
    ///	funcbody ::= `(´ [parlist] `)´ block end
    ///
    ///	parlist ::= namelist [`,´ `...´] | `...´
    ///
    ///	tableconstructor ::= `{´ [fieldlist] `}´
    ///
    ///	fieldlist ::= field {fieldsep field} [fieldsep]
    ///
    ///	field ::= `[´ exp `]´ `=´ exp | Name `=´ exp | exp
    ///
    ///	fieldsep ::= `,´ | `;´
    ///
    ///	binop ::= `+´ | `-´ | `*´ | `/´ | `^´ | `%´ | `..´ |
    ///		 `<´ | `<=´ | `>´ | `>=´ | `==´ | `~=´ |
    ///		 and | or
    ///
    ///	unop ::= `-´ | not | `#´
    /// ```
    pub fn parse(&mut self) -> Result<Vec<StmtNode>> {
        self.next()?;
        let stmts = self.block()?;
        self.check(Token::EOF)?;
        Ok(stmts)
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
        //println!("token:{} => {:?}", self.line_number, self.token);
        Ok(())
    }

    fn look_ahead(&mut self) -> Result<()> {
        debug_assert!(&self.ahead_token == &Token::EOF);
        self.ahead_token = self.scanner.scan()?;
        Ok(())
    }

    fn block_follow(&self) -> bool {
        match self.token {
            Token::Else | Token::Elseif | Token::End | Token::Until | Token::EOF => true,
            _ => false,
        }
    }

    fn block(&mut self) -> Result<Vec<StmtNode>> {
        let mut stmts: Vec<StmtNode> = vec![];
        loop {
            if self.block_follow() {
                break;
            }
            stmts.push(self.statement()?);
            self.testnext(&Token::Char(';'))?;
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
            Token::Return => self.retstat()?,
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
                if self.testnext(&Token::Char('='))? {
                    let val = self.expression()?;
                    Field::new(Some(key), val)
                } else {
                    Field::new(None, key)
                }
            }
            _ => {
                let val = self.expression()?;
                Field::new(None, val)
            }
        };
        Ok(field)
    }

    fn constructor(&mut self) -> Result<ExprNode> {
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
                Token::Char(',') => return Err(self.unexpected(&self.token)),
                _ => break
            }
            self.next()?;
            if !self.testnext(&Token::Char(','))? {
                break;
            }
        }
        Ok(names)
    }

    fn exprlist(&mut self) -> Result<Vec<ExprNode>> {
        let mut exprs = vec![self.expression()?];
        while self.testnext(&Token::Char(','))? {
            let expr = self.expression()?;
            exprs.push(expr);
        }
        Ok(exprs)
    }

    fn funcbody(&mut self) -> Result<ExprNode> {
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

    fn function(&mut self) -> Result<ExprNode> {
        debug_assert!(self.token == Token::Function);
        self.next()?;
        self.funcbody()
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
            _ => return Err(self.unexpected(&self.token))
        };
        self.next()?;
        Ok(expr)
    }

    fn fnargs(&mut self) -> Result<Vec<ExprNode>> {
        let line = self.line_number;
        let mut next = false;
        let expr = match self.token {
            Token::Char('(') => {
                self.next()?;
                let exprs = if self.token != Token::Char(')') {
                    self.exprlist()?
                } else {
                    // no arguments
                    vec![]
                };
                self.check_match(Token::Char(')'), Token::Char('('), line)?;
                exprs
            }
            Token::Char('{') => vec![self.constructor()?],
            Token::String(ref s) => {
                next = true;
                vec![ExprNode::new(Expr::Ident(s.clone()))]
            }
            _ => return Err(Error::SyntaxError("function arguments expected".to_string()))
        };
        if next {
            self.next()?;
        }
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
                        let mut obj = Expr::AttrGet(Box::new(expr), Box::new(key));
                        expr = ExprNode::new(obj);
                    } else {
                        return Err(self.unexpected(&self.token));
                    }
                    self.next()?;
                }
                Token::Char('[') => {
                    self.next()?;
                    let mut key = self.expression()?;
                    let mut obj = Expr::AttrGet(Box::new(expr), Box::new(key));
                    expr = ExprNode::new(obj);
                    self.check_next(Token::Char(']'))?;
                }
                Token::Char(':') => {
                    self.next()?;
                    let method = if let Token::Ident(ref s) = self.token {
                        s.clone()
                    } else {
                        return Err(self.unexpected(&self.token));
                    };
                    self.next()?;
                    let args = self.fnargs()?;
                    let call = MethodCall::new(expr, method, args);
                    expr = ExprNode::new(Expr::MethodCall(Box::new(call)));
                }
                Token::Char('(') | Token::String(_) | Token::Char('{') => {
                    let args = self.fnargs()?;
                    let call = FuncCall::new(expr, args);
                    expr = ExprNode::new(Expr::FuncCall(Box::new(call)));
                }
                _ => return Ok(expr)
            };
        }
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
            Token::Char(c) if c == '{' => return Ok(self.constructor()?),
            Token::Function => return Ok(self.function()?),
            _ => return Ok(self.primaryexp()?)
        };
        self.next()?;
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
        let then = self.block()?;
        Ok(IfThenElse::new(condition, then, vec![]))
    }

    /// ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
    fn ifstat(&mut self, line: u32) -> Result<StmtNode> {
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

    /// ```BNF
    /// whilestat -> WHILE cond DO block END
    /// ```
    fn whilestat(&mut self, line: u32) -> Result<StmtNode> {
        debug_assert!(self.token == Token::While);
        let line = self.line_number;
        self.next()?; // skip WHILE
        let cond = self.expression()?;
        self.check_next(Token::Do)?;
        let stmts = self.block()?;
        self.check_match(Token::End, Token::While, line)?;
        Ok(StmtNode::new(Stmt::While(cond, stmts)))
    }

    fn forbody(&mut self) -> Result<Vec<StmtNode>> {
        self.check_next(Token::Do)?;
        self.block()
    }

    fn fornum(&mut self, varname: String) -> Result<StmtNode> {
        self.check_next(Token::Char('='))?;
        let init = self.expression()?;
        self.check_next(Token::Char(','))?;
        let limit = self.expression()?;
        // skip optional
        let skip = if self.testnext(&Token::Char(','))? {
            self.expression()?
        } else {
            ExprNode::new(Expr::Number(1.0))
        };
        let body = self.forbody()?;
        let stmt = Stmt::NumberFor(NumberFor::new(varname, init, limit, skip, body));
        Ok(StmtNode::new(stmt))
    }

    fn forlist(&mut self, indexname: String) -> Result<StmtNode> {
        let mut names = vec![indexname];
        while self.testnext(&Token::Char(','))? {
            if let Token::Ident(ref s) = self.token {
                names.push(s.clone());
            } else {
                return Err(self.unexpected(&self.token));
            }
            self.next()?;
        }
        self.check_next(Token::In)?;
        let exprs = self.exprlist()?;
        let body = self.forbody()?;
        let stmt = Stmt::GenericFor(GenericFor::new(names, exprs, body));
        Ok(StmtNode::new(stmt))
    }

    /// ```BNF
    /// forstat -> FOR (fornum | forlist) END
    /// ```
    fn forstat(&mut self, line: u32) -> Result<StmtNode> {
        debug_assert!(self.token == Token::For);
        let line = self.line_number;
        self.next()?; // skip FOR
        let varname = if let Token::Ident(ref s) = self.token {
            s.clone()
        } else {
            return Err(Error::SyntaxError("unexpected symbol".to_string()));
        };
        self.next()?;
        let stmt = match self.token {
            Token::Char('=') => self.fornum(varname)?,
            Token::Char(',') | Token::In => self.forlist(varname)?,
            _ => return Err(Error::SyntaxError("= or in expected".to_string()))
        };
        self.check_match(Token::End, Token::For, line);
        Ok(stmt)
    }

    /// ```BNF
    /// repeatstat -> REPEAT block UNTIL cond
    /// ```
    fn repeatstat(&mut self, line: u32) -> Result<StmtNode> {
        debug_assert!(self.token == Token::Repeat);
        let line = self.line_number;
        self.next()?; // skip REPEAT
        let stmts = self.block()?;
        self.check_match(Token::Until, Token::Repeat, line)?;
        let cond = self.expression()?;
        Ok(StmtNode::new(Stmt::Repeat(cond, stmts)))
    }

    fn funcstat(&mut self, line: u32) -> Result<StmtNode> {
        debug_assert!(self.token == Token::Function);
        self.next()?; // skip FUNCTION

        // funcname
        let mut nameexpr = if let Token::Ident(ref s) = self.token {
            ExprNode::new(Expr::String(s.clone()))
        } else {
            return Err(self.unexpected(&self.token));
        };
        self.next()?;

        while self.testnext(&Token::Char('.'))? {
            if let Token::Ident(ref s) = self.token {
                let key = ExprNode::new(Expr::String(s.clone()));
                let mut obj = Expr::AttrGet(Box::new(nameexpr), Box::new(key));
                nameexpr = ExprNode::new(obj);
            } else {
                return Err(self.unexpected(&self.token));
            }
            self.next()?;
        }

        let method = if self.testnext(&Token::Char(':'))? {
            let m = if let Token::Ident(ref s) = self.token {
                Some(s.clone())
            } else {
                return Err(self.unexpected(&self.token));
            };
            self.next()?;
            m
        } else {
            None
        };

        let body = self.funcbody()?;
        let stmt = match method {
            Some(m) => {
                let def = MethodDef::new(nameexpr, m, body);
                Stmt::MethodDef(def)
            }
            None => {
                let def = FuncDef::new(nameexpr, body);
                Stmt::FuncDef(def)
            }
        };
        Ok(StmtNode::new(stmt))
    }

    fn localfunc(&mut self) -> Result<StmtNode> {
        let mut nameexpr = if let Token::Ident(ref s) = self.token {
            ExprNode::new(Expr::String(s.clone()))
        } else {
            return Err(self.unexpected(&self.token));
        };
        self.next()?;
        let body = self.funcbody()?;
        let stmt = Stmt::FuncDef(FuncDef::new(nameexpr, body));
        Ok(StmtNode::new(stmt))
    }

    /// ```BNF
    /// stat -> LOCAL NAME {`,' NAME} [`=' explist1]
    /// ```
    fn localstat(&mut self) -> Result<StmtNode> {
        let namelist = self.namelist()?;
        let exprlist = if self.testnext(&Token::Char('='))? {
            self.exprlist()?
        } else {
            vec![]
        };
        let stmt = Stmt::LocalAssign(namelist, exprlist);
        Ok(StmtNode::new(stmt))
    }

    /// ```BNF
    /// stat -> func | assignment
    /// ```
    fn exprstat(&mut self) -> Result<StmtNode> {
        let expr = self.primaryexp()?;
        let stmt = match expr.inner() {
            &Expr::FuncCall(_) => Stmt::FuncCall(expr),
            &Expr::MethodCall(_) => Stmt::MethodCall(expr),
            _ => {
                let mut lhs = vec![expr];
                while self.testnext(&Token::Char(','))? {
                    lhs.push(self.primaryexp()?)
                }
                self.check_next(Token::Char('='))?;
                let rhs = self.exprlist()?;
                Stmt::Assign(lhs, rhs)
            }
        };
        Ok(StmtNode::new(stmt))
    }

    fn breakstat(&mut self) -> Result<StmtNode> {
        debug_assert!(self.token == Token::Break);
        self.next()?; // skip BREAK
        let stmt = Stmt::Break;
        Ok(StmtNode::new(stmt))
    }

    fn retstat(&mut self) -> Result<StmtNode> {
        debug_assert!(self.token == Token::Return);
        self.next()?; // skip RETURE
        let exprlist = if self.block_follow() || self.token == Token::Char(';') {
            vec![]
        } else {
            self.exprlist()?
        };
        let stmt = Stmt::Return(exprlist);
        Ok(StmtNode::new(stmt))
    }

    fn check_next(&mut self, expect: Token) -> Result<()> {
        self.check(expect)?;
        self.next()
    }

    fn check(&self, expect: Token) -> Result<()> {
        if self.token == expect {
            Ok(())
        } else {
            let s = format!("{}:{}: {} expected", self.source, self.line_number, expect.to_string());
            Err(Error::SyntaxError(s))
        }
    }

    fn check_match(&mut self, what: Token, who: Token, line: u32) -> Result<()> {
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

    fn unexpected(&self, t: &Token) -> Error {
        let err = format!("{}:{}: unexpected: {}", self.source, self.line_number, t.to_string());
        Error::SyntaxError(err)
    }
}