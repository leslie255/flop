use std::{iter::Peekable, rc::Rc};

use crate::{
    source_str::{SourceCharIndices, SourceIndex, SourceStr},
    span::{span, Spanned, ToSpanned},
    token::{tokens, Token},
};

#[allow(dead_code)]
pub fn lex(path: Rc<str>, input: &str) -> Result<Vec<Spanned<Token>>, Spanned<LexerError>> {
    let mut tokens = Vec::<Spanned<Token>>::new();
    let mut lexer = LexerState::new(path, input);
    loop {
        match lexer.poll(&mut tokens) {
            Some(Ok(())) => (),
            Some(Err(e)) => return Err(e),
            None => break,
        }
    }
    Ok(tokens)
}

#[derive(Debug, Clone)]
pub enum LexerError {
    #[allow(dead_code)]
    InvalidChar(char),
}

struct LexerState<'s> {
    path: Rc<str>,
    input: SourceStr<'s>,
    chars: Peekable<SourceCharIndices<'s>>,
}

impl<'s> LexerState<'s> {
    fn new(path: Rc<str>, input: &'s str) -> Self {
        let input = SourceStr::from(input);
        Self {
            path,
            input,
            chars: input.char_indices().peekable(),
        }
    }

    #[must_use]
    fn poll(
        &mut self,
        tokens: &mut Vec<Spanned<Token>>,
    ) -> Option<Result<(), Spanned<LexerError>>> {
        match self.chars.next()? {
            (_, c) if c.is_whitespace() => Some(Ok(())),
            (_, c) if c.is_ascii_digit() => todo!("number literals"),
            (_, '#') => {
                while !matches!(self.chars.next(), Some((_, '\n'))) {}
                self.poll(tokens)
            }
            (_, '\'') => todo!("character literals"),
            (_, '\"') => todo!("string literals"),
            (start, c) if is_ident_body(c) => {
                let end = take_while(start, &mut self.chars, is_ident_body).unwrap_or(start);
                let s = self.input.slice(start..end);
                let t = ident_or_keyword(s);
                tokens.push(t.to_spanned(span!(self.path.clone(), start..end)));
                Some(Ok(()))
            }
            (start, '{') => {
                let token = tokens::BraceL(span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, '}') => {
                let token = tokens::BraceR(span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, '[') => {
                let token = tokens::BracketL(span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, ']') => {
                let token = tokens::BracketR(span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, '(') => {
                let token = tokens::ParenL(span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, ')') => {
                let token = tokens::ParenR(span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, '.') => {
                let token = Token![.](span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, ',') => {
                let token = Token![,](span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, ';') => {
                let token = Token![;](span!(self.path.clone(), start..start));
                tokens.push(token);
                Some(Ok(()))
            }
            (start, c) if is_punct_body(c) => {
                let end = take_while(start, &mut self.chars, is_punct_body).unwrap_or(start);
                let s = self.input.slice(start..end);
                let token = parse_puntuation(s);
                tokens.push(token.to_spanned(span!(self.path.clone(), start..end)));
                Some(Ok(()))
            }
            (i, c) => Some(Err(
                LexerError::InvalidChar(c).to_spanned(span!(self.path.clone(), i..i))
            )),
        }
    }
}

fn take_while(
    start: SourceIndex,
    char_indices: &mut Peekable<SourceCharIndices>,
    mut predicate: impl FnMut(char) -> bool,
) -> Option<SourceIndex> {
    let &(mut end, c) = char_indices.peek()?;
    if !predicate(c) {
        return Some(start);
    }
    char_indices.next();
    while let Some(&(i, c)) = char_indices.peek() {
        if !predicate(c) {
            return Some(end);
        }
        end = i;
        char_indices.next();
    }
    Some(end)
}

fn parse_puntuation(s: &str) -> Token {
    match s {
        "=" => Token::Eq,
        "|" => Token::Verbar,
        ":" => Token::Colon,
        "." => Token::Period,
        "," => Token::Comma,
        ";" => Token::Semicolon,
        "→" => Token::Arrow,
        "(" => Token::ParenL,
        ")" => Token::ParenR,
        "[" => Token::BracketL,
        "]" => Token::BracketR,
        "{" => Token::BraceL,
        "}" => Token::BraceR,
        "∀" => Token::Forall,
        s => Token::Punct(s.into()),
    }
}

fn is_ident_body(c: char) -> bool {
    // FIXME: Handle emojis.
    c.is_alphanumeric() || c == '_' || c == '\''
}

fn is_punct_body(c: char) -> bool {
    (c.is_ascii_punctuation()
        || unicode_blocks::GENERAL_PUNCTUATION.contains(c)
        || unicode_blocks::SUPPLEMENTAL_PUNCTUATION.contains(c)
        || unicode_blocks::MATHEMATICAL_OPERATORS.contains(c)
        || unicode_blocks::SUPPLEMENTAL_MATHEMATICAL_OPERATORS.contains(c)
        || unicode_blocks::ARROWS.contains(c))
        && c != '.'
        && c != '{'
        && c != '}'
        && c != '['
        && c != ']'
        && c != '('
        && c != ')'
        && c != ','
        && c != ';'
}

fn ident_or_keyword(s: &str) -> Token {
    match s {
        "type" => Token::Type,
        s => Token::Ident(s.into()),
    }
}
