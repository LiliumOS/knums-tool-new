use std::borrow::Cow;

use imt::uuid::Uuid;
use lilium_sys::uuid::TryParseUuidError;
use logos::{Lexer, Logos, Skip};

#[derive(Debug, PartialEq, Clone)]
pub enum LexErrorKind {
    Other,
    UnknownToken,
    InvalidUuid(TryParseUuidError),
}

impl core::fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Other => f.write_str("Other Token Error"),
            Self::UnknownToken => f.write_str("Unknown Token"),
            Self::InvalidUuid(_) => f.write_fmt(format_args!("Bad UUID Literal")),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LexError<'src> {
    pub span: Span,
    pub token: Cow<'src, str>,
    pub error_kind: LexErrorKind,
}

impl<'src> LexError<'src> {
    pub fn into_owned(self) -> LexError<'static> {
        LexError {
            span: self.span,
            token: Cow::Owned(self.token.into_owned()),
            error_kind: self.error_kind,
        }
    }
}

impl<'src> core::fmt::Display for LexError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "[{}-{}]: {} {}",
            self.span.start,
            self.span.end,
            self.error_kind,
            self.token.escape_default()
        ))
    }
}

impl core::error::Error for LexError<'static> {}

impl<'src> Default for LexError<'src> {
    fn default() -> Self {
        Self {
            span: Span::default(),
            token: Cow::Borrowed(""),
            error_kind: LexErrorKind::Other,
        }
    }
}

/// Token type for knums
///
/// ```abnf
/// ; Whitespace Sensitive Grammer
///
/// file := [<whitespace>] *(<token> [<whitespace>])
///     ; Every file must match the lexical file non-terminal
///
/// whitespace := 1*(<White_Space> / <comment>)
///
/// newline := %x0A
/// comment-begin := %x00-09 / %x0B-20 / %x22-2E / %x30-x10FFFF
/// comment-char := <comment-begin> / "!" / "/"
///
/// comment := "//" [<comment-begin> [*<comment-char>]] <newline>
///
/// doc-comment := "///" *<comment-char> <newline>
/// inner-doc-comment := "//!" *<comment-char> <newline>
///
/// ident := <XID_Start> *<XID_Continue> ; Except <keyword>
///
/// keyword := "use" / "type" / "const" / "mut" / "handle" / "shared_handle" / "struct" / "union"
///
/// octal-digit := %x30-37
///
/// digit := %x30-39
///
/// hex-digit := <digit> / %x41-46 / %x61-66
///
/// uuid := "U{" 8<hex-digit> "-" 4<hex-digit> "-" 4<hex-digit> "-" 4<hex-digit> "-" 12<hex-digit> "}
///
/// hex-literal = "0" ("x" / "X") *(<hex-digit> ["_"]) <hex-digit>
///
/// dec-literal = *(<digit> ["_"]) <digit>
///
/// oct-literal = "0 ("o" / "O") *(<octal-digit> ["_"]) <octal-digit>
///
/// int-literal = <hex-literal> / <dec-literal> / <oct-literal>
///
/// punct := "=" / "*" / "+" / "-" / "^" / "&" / "|" / "<<" / ">>" / "<" / ">" / "->" / "{" / "}" / "[" / "]" / "(" / ")" / "/"
///
/// ascii-ident-begin := %x41-5A / %x61-7A / "_"
///
/// ascii-ident-continue := <ascii-ident-begin> / %x30-39
///
/// direcitve := "%" <ascii-ident-begin> *<ascii-ident-continue>
///     ; Must only be followed by `<White_Space>` or `<comment>` before first `<newline>`
///
/// token := <int-literal> / <uuid> / <punct> / <keyword> / <ident> / <doc-comment> / <inner-doc-comment> / <directive>
/// ```
#[derive(Logos, Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[logos(error(LexError<'s>, invalid_token))]
pub enum Token<'s> {
    #[regex("(0x([0-9A-Fa-f] _?)*[0-9A-Fa-f]) / (0o([0-7] _?)* [0-7]) / ([0-9] _?)*[0-9]")]
    IntLiteral(&'s str),
    #[regex("[\\p{XID_Start}_][\\p{XID_Continue}_]*")]
    Identifier(&'s str),
    #[regex("\"([^\"\\\\] | \\\\[\\\\\"nrt\n])*\"")]
    StringLiteral(&'s str),
    #[token("use")]
    Use,
    #[token("type")]
    Type,
    #[token("const")]
    Const,
    #[token("mut")]
    Mut,
    #[token("handle")]
    Handle,
    #[token("shared_handle")]
    SharedHandle,
    #[token("struct")]
    Struct,
    #[token("union")]
    Union,
    #[token("fn")]
    Fn,
    #[token("enum")]
    Enum,
    #[regex("U\\{[^\\}]*\\}", callback = parse_uuid)]
    UuidLiteral(Uuid),
    #[regex("%[[[:alpha:]]_][[[:alnum:]]_]*[\\p{White_Space}&&[^\\n\\r]]*\\r?\\n", callback = |lexer| lexer.slice().trim())]
    Directive(&'s str),
    #[token("///", callback = parse_doc)]
    Doc(&'s str),
    #[token("//!", callback = parse_doc)]
    InnerDoc(&'s str),
    #[token("//", callback = parse_comment)]
    Comment,
    #[regex("[\\p{White_Space}&&[^\\r\\n]]", callback = logos::skip)]
    Whitespace,
    #[token("\n", callback = logos::skip, priority = 10)]
    #[token("\r\n", callback = logos::skip, priority = 10)]
    NewLine,
    #[token("=")]
    Equals,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("<<")]
    LeftShift,
    #[token(">>")]
    RightShift,
    #[token("^")]
    Caret,
    #[token("&")]
    And,
    #[token("|")]
    Or,
    #[token("<")]
    OpenAngle,
    #[token(">")]
    CloseAngle,
    #[token("[")]
    OpenSquare,
    #[token("]")]
    CloseSquare,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("->")]
    Arrow,
    #[token("!")]
    Bang,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
}

pub fn parse_doc<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> &'src str {
    let p = lexer.remainder();

    for (i, c) in p.char_indices() {
        if c == '\n' || c == '\r' {
            let (string, _) = p.split_at(i);
            lexer.bump(i);
            return string;
        }
    }

    p
}

pub fn parse_comment<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> Skip {
    let p = lexer.remainder();

    for (i, c) in p.char_indices() {
        if c == '\n' || c == '\r' {
            lexer.bump(i);
            return Skip;
        }
    }

    Skip
}

fn invalid_token<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> LexError<'src> {
    let tok = lexer.remainder();
    let begin = lexer.span().end;
    for (i, c) in tok.char_indices() {
        if c.is_whitespace() {
            let string = &tok[..i];
            return LexError {
                span: begin..(begin + i),
                token: Cow::Borrowed(string),
                error_kind: LexErrorKind::UnknownToken,
            };
        }
    }
    LexError {
        span: begin..(begin + tok.len()),
        token: Cow::Borrowed(tok),
        error_kind: LexErrorKind::UnknownToken,
    }
}

fn parse_uuid<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> Result<Uuid, LexError<'src>> {
    lilium_sys::uuid::try_parse_uuid(&lexer.slice()[1..])
        .map_err(|e| LexError {
            span: lexer.span(),
            token: Cow::Borrowed(lexer.slice()),
            error_kind: LexErrorKind::InvalidUuid(e),
        })
        .map(Uuid)
}

pub use logos::Span;

pub fn lex_file<'src>(file: &'src str) -> Result<Vec<Token<'src>>, LexError<'src>> {
    Token::lexer(file)
        .spanned()
        .map(|(tok, _)| match tok {
            Ok(tok) => Ok(tok),
            Err(e) => Err(e),
        })
        .collect()
}
