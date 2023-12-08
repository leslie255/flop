use crate::symbol::FnId;
use crate::token::Token;
use crate::ty::{Ty, Ty_, Signess, IntSize, FloatSize};
use crate::{token::TokenStream, ty::FnType};
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::iter::Peekable;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Variable(VarId),
    InvalidVariable(Rc<str>),
    IntLiteral(u64),
    FloatLiteral(f64),
    VarDecl(VarId, Option<Ty>, Option<Box<AstNode>>),
    BinaryOp(BinaryOpKind, Box<AstNode>, Box<AstNode>),
    UnaryOp(UnaryOpKind, Box<AstNode>),
    Assign(Box<AstNode>, Box<AstNode>),
    TupleLiteral(Vec<AstNode>),
    FnDef {
        name: Rc<str>,
        /// `VarId`s of the arguments.
        arg_vars: Vec<VarId>,
        sig: FnType,
        body: Vec<AstNode>,
    },
    ExternFn(Rc<str>, FnType),
    Return(Option<Box<AstNode>>),
    IfElse(Box<AstNode>, Vec<AstNode>, Vec<AstNode>),
    Call(Callee, Vec<AstNode>),
}

impl AstNode {
    /// Shorthand for creating an empty tuple.
    pub const fn empty() -> Self {
        Self::TupleLiteral(Vec::new())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Ne,
    Le,
    LeEq,
    Gr,
    GrEq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpKind {
    Deref,
    Ref,
    Not,
    /// Unary add.
    Add,
    /// Unary sub.
    Sub,
}

/// `VarId`s are unique among its function.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(usize);
impl Debug for VarId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "var{}", self.0)
    }
}
impl index_vec::Idx for VarId {
    fn from_usize(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

#[derive(Clone, PartialEq)]
pub enum Callee {
    /// All calls have unknown callee at parsing stage.
    /// However in later stages this is considered an error.
    Unknown(Box<AstNode>),
    Static(FnId),
}
impl Debug for Callee {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown(s) => write!(f, "unknown_fn({:?})", s),
            Self::Static(id) => Debug::fmt(id, f),
        }
    }
}

macro_rules! expect_token {
    ($t:expr, $expect:pat, $($reason:tt)*) => {
        assert!($t.is_some_and(|t| matches!(t, $expect)), $($reason)*);
    };
    ($t:expr, $expect:pat $(,)?) => {
        assert!($t.is_some_and(|t| matches!(t, $expect)));
    };
}

#[derive(Debug, Clone)]
struct ParsingContext {
    variables: Vec<HashMap<Rc<str>, VarId>>,
    counters: Vec<usize>,
}
impl ParsingContext {
    pub const fn new() -> Self {
        Self {
            variables: Vec::new(),
            counters: Vec::new(),
        }
    }
    pub fn enter_block(&mut self) {
        self.variables.push(HashMap::new());
    }
    /// Panics if not currently in any blocks.
    pub fn leave_block(&mut self) {
        assert!(
            self.variables.pop().is_some(),
            "Called `ParsingContext::leaves_block` when not appearing to be in a block"
        )
    }
    /// Has to also call `enter_block` later.
    pub fn enters_fn(&mut self) {
        self.counters.push(0);
    }
    /// Has to also call `enter_block` later.
    /// Panics if not currently in any functions.
    pub fn leaves_fn(&mut self) {
        assert!(
            self.counters.pop().is_some(),
            "Called `ParsingContext::leaves_fn` when not appearing to be in a function"
        );
    }
    /// Has to also call `enter_block` later.
    /// Get the variable ID for a variable name.
    pub fn var_id(&self, var_name: &str) -> Option<VarId> {
        self.variables
            .iter()
            .rev()
            .find_map(|vars| vars.get(var_name).copied())
    }
    /// Panics if not currently in any blocks.
    pub fn declare_var(&mut self, var_name: Rc<str>) -> VarId {
        let counter = self
            .counters
            .last_mut()
            .expect("Called `ParsingContext::declare_var` when not appearing to be in a function");
        let var_id = VarId(*counter);
        *counter += 1;
        self.variables
            .last_mut()
            .expect("Called `ParsingContext::declare_var` when not appearing to be in a block")
            .insert(var_name, var_id);
        var_id
    }
}
impl Default for ParsingContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse a block, starting at the iterator pointing at the `{` token
#[must_use]
fn parse_block(
    tokens: &mut Peekable<TokenStream>,
    ctx: &mut ParsingContext,
) -> Option<Vec<AstNode>> {
    ctx.enter_block();
    let mut body = Vec::<AstNode>::new();
    while !tokens.peek()?.is_brace_close() {
        let expr = parse_expr(15, tokens, ctx)?;
        match tokens.peek()? {
            Token::Semicolon => {
                tokens.next();
            }
            Token::BraceClose => {
                todo!("tail logic");
            }
            _ => (),
        }
        body.push(expr);
    }
    tokens.next(); // the `}` token
    ctx.leave_block();
    Some(body)
}

/// Parse a function definition, starting from the iterator pointing at the `fn` token
#[must_use]
fn parse_fn(tokens: &mut Peekable<TokenStream>, ctx: &mut ParsingContext) -> Option<AstNode> {
    // Name of function.
    let name = tokens
        .next()?
        .into_id()
        .expect("Expects identifier after `fn`");
    expect_token!(tokens.next(), Token::ParenOpen);

    ctx.enters_fn();
    ctx.enter_block();
    let mut arg_vars = Vec::<VarId>::new();
    let mut arg_tys = Vec::<Ty>::new();

    // Arguments.
    while !tokens.peek()?.is_paren_close() {
        let arg_name = tokens
            .next()
            .and_then(Token::into_id)
            .expect("Expects argument name");
        expect_token!(tokens.next(), Token::Colon);
        arg_vars.push(ctx.declare_var(arg_name));
        let ty = parse_ty(tokens, ctx)?;
        arg_tys.push(ty);
        match tokens.peek()? {
            Token::Comma => {
                tokens.next();
            }
            Token::ParenClose => (),
            _ => panic!("Expects `,` or `)`"),
        }
    }
    tokens.next();

    let ret_ty = match tokens.peek() {
        Some(Token::Arrow) => {
            tokens.next();
            parse_ty(tokens, ctx)?
        }
        _ => Ty_::Tuple(Vec::new()).into(),
    };

    expect_token!(
        tokens.next(),
        Token::BraceOpen,
        "Expects block after function declaration"
    );
    let mut body = Vec::<AstNode>::new();
    while !tokens.peek()?.is_brace_close() {
        let expr = parse_expr(15, tokens, ctx)?;
        match tokens.peek()? {
            Token::Semicolon => {
                tokens.next();
            }
            Token::BraceClose => {
                todo!("tail logic");
            }
            _ => (),
        }
        body.push(expr);
    }
    tokens.next(); // the `}` token
    ctx.leave_block();
    ctx.leaves_fn();

    Some(AstNode::FnDef {
        name,
        arg_vars,
        sig: FnType {
            inputs: arg_tys,
            outputs: ret_ty,
        },
        body,
    })
}

/// Parse a let expression, starting from the iterator pointing at the `let` token
#[must_use]
fn parse_let(tokens: &mut Peekable<TokenStream>, ctx: &mut ParsingContext) -> Option<AstNode> {
    let var_name = tokens
        .next()?
        .into_id()
        .expect("Expect identifier as LHS of `let`");
    let lhs = ctx.declare_var(var_name);
    let ty = match tokens.peek() {
        Some(Token::Colon) => {
            tokens.next();
            Some(parse_ty(tokens, ctx)?)
        }
        _ => None,
    };
    let rhs = match tokens.peek() {
        Some(Token::Eq) => {
            tokens.next();
            Some(parse_expr(15, tokens, ctx)?)
        }
        _ => None,
    };
    Some(AstNode::VarDecl(lhs, ty, rhs.map(Box::new)))
}

/// Parse arguments to a function call, starting from the iterator pointing at the token before `(`
#[must_use]
fn parse_call_args(
    tokens: &mut Peekable<TokenStream>,
    ctx: &mut ParsingContext,
) -> Option<Vec<AstNode>> {
    tokens.next();
    let mut args = Vec::<AstNode>::new();
    while !tokens.peek()?.is_paren_close() {
        args.push(parse_expr(15, tokens, ctx)?);
        match tokens.peek()? {
            Token::Comma => {
                tokens.next();
            }
            Token::ParenClose => (),
            _ => panic!("Expects `,` or `)`"),
        }
    }
    tokens.next();
    Some(args)
}

/// Parse if statements, starting from the iterator pointing at the token `if`
#[must_use]
fn parse_if(tokens: &mut Peekable<TokenStream>, ctx: &mut ParsingContext) -> Option<AstNode> {
    let condition = parse_expr(15, tokens, ctx)?;
    assert!(tokens.next()?.is_brace_open(), "Expect `{{`");
    let if_body = parse_block(tokens, ctx)?;
    let else_body = match tokens.peek() {
        Some(Token::Else) => {
            tokens.next();
            assert!(tokens.next()?.is_brace_open(), "Expect `{{`");
            parse_block(tokens, ctx)?
        }
        _ => Vec::new(),
    };
    Some(AstNode::IfElse(Box::new(condition), if_body, else_body))
}

#[must_use]
fn parse_paren(tokens: &mut Peekable<TokenStream>, ctx: &mut ParsingContext) -> Option<AstNode> {
    let node = parse_expr(15, tokens, ctx)?;
    match tokens.next()? {
        Token::Comma => {
            // is a tuple
            let mut fields = vec![node];
            while !tokens.peek()?.is_paren_close() {
                fields.push(parse_expr(15, tokens, ctx)?);
                match tokens.peek()? {
                    Token::Comma => {
                        tokens.next();
                    }
                    Token::ParenClose => (),
                    _ => panic!("Expects `)` or `,`"),
                }
            }
            tokens.next();
            Some(AstNode::TupleLiteral(fields))
        }
        Token::ParenOpen => Some(node),
        _ => panic!("Expects `)` or `,`"),
    }
}

fn parse_return(tokens: &mut Peekable<TokenStream>, ctx: &mut ParsingContext) -> Option<AstNode> {
    match tokens.peek()? {
        Token::Semicolon | Token::BraceClose => Some(AstNode::Return(None)),
        _ => Some(AstNode::Return(Some(Box::new(parse_expr(
            15, tokens, ctx,
        )?)))),
    }
}

fn parse_ty(tokens: &mut Peekable<TokenStream>, ctx: &mut ParsingContext) -> Option<Ty> {
    match tokens.next()? {
        Token::Id(s) => match s.as_ref() {
            "u64" => Some(Ty_::Int(Signess::Unsigned, IntSize::_64).into()),
            "u32" => Some(Ty_::Int(Signess::Unsigned, IntSize::_16).into()),
            "u16" => Some(Ty_::Int(Signess::Unsigned, IntSize::_32).into()),
            "u8"  => Some(Ty_::Int(Signess::Unsigned, IntSize::_8).into()),
            "i64" => Some(Ty_::Int(Signess::Signed, IntSize::_64).into()),
            "i32" => Some(Ty_::Int(Signess::Signed, IntSize::_16).into()),
            "i16" => Some(Ty_::Int(Signess::Signed, IntSize::_32).into()),
            "i8"  => Some(Ty_::Int(Signess::Signed, IntSize::_8).into()),
            "f64" => Some(Ty_::Float(FloatSize::_64).into()),
            "f32" => Some(Ty_::Float(FloatSize::_32).into()),
            "bool" => Some(Ty_::Bool.into()),
            "never" => Some(Ty_::Never.into()),
            s => panic!("Invalid type name {s:?}"),
        },
        Token::Mul => Some(Ty_::Ptr(parse_ty(tokens, ctx)?).into()),
        Token::ParenOpen => {
            let mut tys = Vec::<Ty>::new();
            while !tokens.peek()?.is_paren_close() {
                let ty = parse_ty(tokens, ctx)?;
                tys.push(ty);
                match tokens.peek()? {
                    Token::Comma => {
                        tokens.next();
                    }
                    Token::ParenClose => (),
                    _ => panic!("Expects `,` or `)`"),
                }
            }
            Some(Ty_::Tuple(tys).into())
        }
        _ => panic!("Expects type expression"),
    }
}

#[must_use]
fn parse_expr(
    precedence: u8,
    tokens: &mut Peekable<TokenStream>,
    ctx: &mut ParsingContext,
) -> Option<AstNode> {
    macro_rules! parse_unary_rtl {
        ($precedence:expr , $op_kind:expr) => {{
            let node = parse_expr($precedence, tokens, ctx).expect("Unexpected EOF");
            AstNode::UnaryOp($op_kind, Box::new(node))
        }};
    }
    let node = match tokens.next()? {
        Token::Id(name) => ctx.var_id(&name).map_or_else(|| AstNode::InvalidVariable(name), |id| AstNode::Variable(id)),
        Token::Num(num) => AstNode::IntLiteral(num),
        Token::Exc => parse_unary_rtl!(2, UnaryOpKind::Not),
        Token::Add => parse_unary_rtl!(2, UnaryOpKind::Add),
        Token::Sub => parse_unary_rtl!(2, UnaryOpKind::Sub),
        Token::If => parse_if(tokens, ctx)?,
        Token::Else => panic!("Unexpected `else`"),
        Token::Loop => todo!("loops"),
        Token::Fn => parse_fn(tokens, ctx)?,
        Token::Let => parse_let(tokens, ctx)?,
        Token::Return => parse_return(tokens, ctx)?,
        Token::Break => todo!("loops"),
        Token::Continue => todo!("loops"),
        Token::ExcEq => panic!("Unexpected `!=`"),
        Token::Eq => panic!("Unexpected `=`"),
        Token::EqEq => panic!("Unexpected `==`"),
        Token::Le => panic!("Unexpected `<`"),
        Token::LeEq => panic!("Unexpected `<=`"),
        Token::Gr => panic!("Unexpected `>`"),
        Token::GrEq => panic!("Unexpected `>=`"),
        Token::Mul => unimplemented!("Dereference expressions (`*expr`) is not supported, or maybe it was just an accidental `*`?"),
        Token::Div => panic!("Unexpected `/`"),
        Token::Mod => panic!("Unexpected `%`"),
        Token::And => unimplemented!("Reference expressions (`&expr`) is not supported, or maybe it was just an accidental `&`?"),
        Token::Or => panic!("Unexpected `|`"),
        Token::Comma => panic!("Unexpected `,`"),
        Token::Dot => panic!("Unexpected `.`"),
        Token::Semicolon => parse_expr(15, tokens, ctx)?,
        Token::Colon => panic!("Unexpected `:`"),
        Token::Arrow => panic!("Unexpected `->`"),
        Token::ParenOpen => parse_paren(tokens, ctx)?,
        Token::ParenClose => panic!("Unexpected `)`"),
        Token::SquareOpen => todo!("Arrays"),
        Token::SquareClose => panic!("Unexpected `]`"),
        Token::BraceOpen => todo!("Arbitrary blocks"),
        Token::BraceClose => panic!("Unexpected `}}`"),
    };
    macro_rules! parse_bin_op {
        ($precedence:expr, $op_kind:expr $(,)?) => {
            if precedence >= $precedence {
                tokens.next();
                let rhs = parse_expr($precedence, tokens, ctx).expect("Incomplete RHS");
                AstNode::BinaryOp($op_kind, Box::new(node), Box::new(rhs))
            } else {
                node
            }
        };
    }
    let expr = match tokens.peek() {
        Some(Token::Eq) => {
            if precedence >= 14 {
                tokens.next();
                let rhs = parse_expr(14, tokens, ctx).expect("Incomplete RHS");
                AstNode::Assign(Box::new(node), Box::new(rhs))
            } else {
                node
            }
        }
        Some(Token::EqEq) => parse_bin_op!(7, BinaryOpKind::Eq),
        Some(Token::ExcEq) => parse_bin_op!(7, BinaryOpKind::Ne),
        Some(Token::Le) => parse_bin_op!(6, BinaryOpKind::Le),
        Some(Token::LeEq) => parse_bin_op!(6, BinaryOpKind::LeEq),
        Some(Token::Gr) => parse_bin_op!(6, BinaryOpKind::Gr),
        Some(Token::GrEq) => parse_bin_op!(6, BinaryOpKind::GrEq),
        Some(Token::Add) => parse_bin_op!(4, BinaryOpKind::Add),
        Some(Token::Sub) => parse_bin_op!(4, BinaryOpKind::Sub),
        Some(Token::Mul) => parse_bin_op!(3, BinaryOpKind::Mul),
        Some(Token::Div) => parse_bin_op!(3, BinaryOpKind::Div),
        Some(Token::Mod) => parse_bin_op!(3, BinaryOpKind::Mod),
        Some(Token::And) => parse_bin_op!(11, BinaryOpKind::And),
        Some(Token::Or) => parse_bin_op!(13, BinaryOpKind::Or),
        Some(Token::Dot) => panic!("Member accessing is not supported"),
        Some(Token::ParenOpen) => AstNode::Call(
            Callee::Unknown(Box::new(node)),
            parse_call_args(tokens, ctx)?,
        ),
        _ => node,
    };
    Some(expr)
}

#[derive(Clone, Debug)]
pub struct AstParser<'a> {
    tokens: Peekable<TokenStream<'a>>,
}

impl<'a> AstParser<'a> {
    pub fn new(tokens: Peekable<TokenStream<'a>>) -> Self {
        Self { tokens }
    }
}

impl Iterator for AstParser<'_> {
    type Item = AstNode;

    fn next(&mut self) -> Option<Self::Item> {
        let mut parsing_ctx = ParsingContext::new();
        let expr = parse_expr(15, &mut self.tokens, &mut parsing_ctx);
        if let Some(Token::Semicolon) = self.tokens.peek() {
            self.tokens.next();
        }
        expr
    }
}
