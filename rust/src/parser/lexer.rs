use logos::{FilterResult, Lexer, Logos, Skip};
use std::str::FromStr;

use super::{internalast::Number, LexingError};

#[derive(Clone, Copy)]
pub struct Extras {}

impl Extras {
    pub fn new() -> Extras {
        Extras {}
    }
}

fn newline<'s, T: Logos<'s, Extras = Extras>>(_lex: &mut Lexer<'s, T>) {}

#[derive(Debug, Logos)]
#[logos(extras = Extras, error = LexingError)]
enum StringToken {
    #[token(r"\a",                  |_| '\x07')]
    #[token(r"\",                   |_| '\x08')]
    #[token(r"\f",                  |_| '\x0C')]
    #[token(r"\n",                  |_| '\n')]
    #[regex(r"\\(\r|\n|\r\n|\n\r)", |lex| { newline(lex); '\n' })]
    #[token(r"\r",                  |_| '\r')]
    #[token(r"\t",                  |_| '\t')]
    #[token(r"\v",                  |_| '\x0B')]
    #[token(r"\\",                  |_| '\\')]
    #[token(r#"\""#,                |_| '\"')]
    #[token(r"\'",                  |_| '\'')]
    #[token(r"\[",                  |_| '[')]
    #[token(r"\]",                  |_| ']')]
    /* \x \u \z TODO */
    Escaped(char),

    #[regex(r#"('|")"#)]
    Quote,

    #[regex(r#"[^\\\n\r'"]+"#)]
    Text,
}

fn string(lex: &mut Lexer<Token>) -> Result<String, LexingError> {
    let quote = lex.slice();
    let mut strlex = StringToken::lexer_with_extras(lex.remainder(), lex.extras);
    let mut str = String::new();
    loop {
        use StringToken::*;
        match strlex.next() {
            Some(Ok(Quote)) if strlex.slice() == quote => {
                lex.bump(strlex.span().end);
                lex.extras = strlex.extras;
                return Ok(str);
            }
            Some(Err(e)) => {
                lex.bump(strlex.span().end);
                lex.extras = strlex.extras;
                return Err(e);
            }
            None => {
                lex.bump(strlex.span().end);
                lex.extras = strlex.extras;
                return Err(Default::default());
            }
            Some(Ok(Escaped(c))) => str.push(c),
            Some(Ok(Text | Quote)) => str.push_str(strlex.slice()),
        }
    }
}

#[derive(Logos)]
#[logos(extras = Extras, error = LexingError)]
enum LongStringToken {
    #[regex(r"\\(\r|\n|\r\n|\n\r)", newline)]
    Newline,

    #[token(r"]=*")]
    BracketEqs,

    #[regex(r#"[^\n\r\]]+"#)]
    Text,
}

fn long_string(lex: &mut Lexer<Token>) -> Result<String, LexingError> {
    let len = lex.slice().len();
    let mut strlex = LongStringToken::lexer_with_extras(lex.remainder(), lex.extras);
    let mut res = String::new();
    loop {
        use LongStringToken::*;
        match strlex.next() {
            Some(Ok(BracketEqs))
                if strlex.remainder().chars().next() == Some(']')
                    && strlex.slice().len() + 1 == len =>
            {
                lex.bump(strlex.span().end + 1);
                lex.extras = strlex.extras;
                return Ok(res);
            }
            Some(Err(e)) => {
                lex.bump(strlex.span().end);
                lex.extras = strlex.extras;
                return Err(e);
            }
            None => {
                lex.bump(strlex.span().end);
                lex.extras = strlex.extras;
                return Err(Default::default());
            }
            Some(Ok(Newline)) => res.push('\n'),
            Some(Ok(Text | BracketEqs)) => res.push_str(strlex.slice()),
        }
    }
}

fn long_comment(lex: &mut Lexer<Token>) -> FilterResult<(), LexingError> {
    let len = lex.slice().len() - 2;
    let mut cmtlex = LongStringToken::lexer_with_extras(lex.remainder(), lex.extras);
    loop {
        use {FilterResult::*, LongStringToken::*};
        match cmtlex.next() {
            Some(Ok(BracketEqs))
                if cmtlex.remainder().chars().next() == Some(']')
                    && cmtlex.slice().len() + 1 == len =>
            {
                lex.bump(cmtlex.span().end + 1);
                lex.extras = cmtlex.extras;
                return Skip;
            }
            Some(Err(e)) => {
                lex.bump(cmtlex.span().end);
                lex.extras = cmtlex.extras;
                return Error(e);
            }
            None => {
                lex.bump(cmtlex.span().end);
                lex.extras = cmtlex.extras;
                return Error(Default::default());
            }
            Some(Ok(Text | BracketEqs | Newline)) => (),
        }
    }
}

#[derive(Logos)]
#[logos(extras = Extras, error = LexingError)]
enum CommentToken {
    #[regex(r"[^\r\n]*(\r|\n|\r\n|\n\r)", newline)]
    #[regex(r"[^\r\n]*", logos::skip)]
    Comment,
}

fn comment(lex: &mut Lexer<Token>) -> Skip {
    let mut cmtlex = CommentToken::lexer_with_extras(lex.remainder(), lex.extras);
    cmtlex.next().unwrap().unwrap();
    lex.bump(cmtlex.span().end);
    lex.extras = cmtlex.extras;
    Skip
}

fn number(lex: &mut Lexer<Token>) -> Option<Number> {
    let i = i64::from_str(lex.slice()).ok().map(Number::Integer);
    i.or_else(|| f64::from_str(lex.slice()).ok().map(Number::Float))
}

fn hex(lex: &mut Lexer<Token>) -> Option<Number> {
    Some(Number::Integer(
        i64::from_str_radix(&lex.slice()[2..], 16).ok()?,
    ))
}

#[derive(Logos, Debug, Clone)]
#[logos(extras = Extras, error = LexingError)]
pub enum Token {
    #[regex(r"(\r|\n|\r\n|\n\r)", |lex| newline(lex); Skip)]
    #[regex(r"[ \t]+", logos::skip)]
    #[regex(r"--\[=*\[", long_comment)]
    #[token(r"--", comment)]
    #[token("and")]
    AND,

    #[token("break")]
    BREAK,

    #[token("do")]
    DO,

    #[token("else")]
    ELSE,

    #[token("elseif")]
    ELSEIF,

    #[token("end")]
    END,

    #[token("false")]
    FALSE,

    #[token("for")]
    FOR,

    #[token("function")]
    FUNCTION,

    #[token("goto")]
    GOTO,

    #[token("if")]
    IF,

    #[token("in")]
    IN,

    #[token("local")]
    LOCAL,

    #[token("nil")]
    NIL,

    #[token("not")]
    NOT,

    #[token("or")]
    OR,

    #[token("repeat")]
    REPEAT,

    #[token("return")]
    RETURN,

    #[token("then")]
    THEN,

    #[token("true")]
    TRUE,

    #[token("until")]
    UNTIL,

    #[token("while")]
    WHILE,

    #[regex("[_a-zA-Z][_a-zA-Z0-9]*", |lex| lex.slice().to_owned())]
    NAME(String),

    #[regex(r"[0-9]+\.[0-9]*([eE][0-9]+)?", number)]
    #[regex(r"\.[0-9]+([eE][0-9]+)?", number)]
    #[regex(r"[0-9]+([eE][0-9]+)?", number)]
    #[regex(r"0[xX][0-9A-Fa-f]+", hex)]
    Number(Number),

    #[token("\"", string)]
    #[token("'", string)]
    #[regex(r"\[=*\[", long_string)]
    LITERALSTRING(String),

    #[token("=")]
    EQUAL,

    #[token("::")]
    DOUBLECOLON,

    #[token(":")]
    COLON,

    #[token(";")]
    SEMICOLON,

    #[token(",")]
    COMMA,

    #[token("...")]
    TRIPLEDOT,

    #[token("..")]
    DOUBLEDOT,

    #[token(".")]
    DOT,

    #[token("(")]
    LPAREN,

    #[token(")")]
    RPAREN,

    #[token("+")]
    PLUS,

    #[token("-")]
    HYPHEN,

    #[token("*")]
    ASTERIX,

    #[token("//")]
    DOUBLESLASH,

    #[token("/")]
    SLASH,

    #[token("^")]
    HAT,

    #[token("%")]
    PERCENT,

    #[token("&")]
    AMPERSAND,

    #[token("~")]
    TILDA,

    #[token("|")]
    VERTICALBAR,

    #[token("<<")]
    DOUBLELT,

    #[token(">>")]
    DOUBLEGT,

    #[token("<")]
    LT,

    #[token("<=")]
    LTEQ,

    #[token(">")]
    GT,

    #[token(">=")]
    GTEQ,

    #[token("==")]
    DOUBLEEQUAL,

    #[token("~=")]
    TILDAEQUAL,

    #[token("#")]
    SHARP,

    #[token("{")]
    LBRACE,

    #[token("}")]
    RBRACE,

    #[token("[")]
    LBRACKET,

    #[token("]")]
    RBRACKET,
}
