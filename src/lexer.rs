use std::{borrow::Cow, time::Instant};

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
            Self::UnknownToken => f.write_str("Lexing Error: Unknown Token"),
            Self::InvalidUuid(_) => f.write_fmt(format_args!("Bad UUID Literal")),
        }
    }
}

#[derive(PartialEq, Clone, Copy, Eq, Hash, Default)]
pub struct TextSpan {
    pub begin: Pos,
    pub end: Pos,
}

impl core::fmt::Display for TextSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}-{}", self.begin, self.end))
    }
}

impl core::fmt::Debug for TextSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}-{:?}", self.begin, self.end))
    }
}

#[derive(PartialEq, Clone, Copy, Eq, Hash, Default)]
pub struct Pos {
    pub row: u32,
    pub col: u32,
    pub byte_pos: usize,
}

impl core::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.row, self.col))
    }
}

impl core::fmt::Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}({})", self.row, self.col, self.byte_pos))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LexError<'src> {
    pub span: TextSpan,
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
            "[{}]: {} {}",
            self.span,
            self.error_kind,
            self.token.escape_default()
        ))
    }
}

impl core::error::Error for LexError<'static> {}

impl<'src> Default for LexError<'src> {
    fn default() -> Self {
        Self {
            span: TextSpan::default(),
            token: Cow::Borrowed(""),
            error_kind: LexErrorKind::Other,
        }
    }
}

pub struct LexExtras {
    cur_line: u32,
    line_span_start: usize,
    linemap: Vec<usize>,
}

impl Default for LexExtras {
    fn default() -> Self {
        Self {
            cur_line: 0,
            line_span_start: 0,
            linemap: vec![0],
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
#[logos(error(LexError<'s>, default_error))]
#[logos(extras = LexExtras)]
pub enum Token<'s> {
    #[regex("0x[0-9A-Fa-f](_?[0-9A-Fa-f])*")]
    #[regex("0o[0-7](_?[0-7])*")]
    #[regex("[1-9](_?[0-9])*")]
    #[token("0", priority = 9)]
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
    #[token("\n", callback = next_line, priority = 10)]
    #[token("\r\n", callback = next_line, priority = 10)]
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
    #[token("[^\\p{White_Space}]+", priority = 1, callback = invalid_token)]
    Err,
}

impl<'s> Token<'s> {
    pub const fn class(&self) -> &'static str {
        match self {
            Token::IntLiteral(_) => "int literal",
            Token::Identifier(_) => "identifier",
            Token::StringLiteral(_) => "string literal",
            Token::Use => "use",
            Token::Type => "type",
            Token::Const => "const",
            Token::Mut => "mut",
            Token::Handle => "handle",
            Token::SharedHandle => "shared_handle",
            Token::Struct => "struct",
            Token::Union => "union",
            Token::Fn => "fn",
            Token::Enum => "enum",
            Token::UuidLiteral(_) => "a uuid",
            Token::Directive(_) => "a directive",
            Token::Doc(_) => "a doc comment",
            Token::InnerDoc(_) => "inner doc",
            Token::Comment => "comment",
            Token::Whitespace => "whitespace",
            Token::NewLine => "white space",
            Token::Equals => "`=`",
            Token::Plus => "`+`",
            Token::Minus => "`-`",
            Token::Star => "`*`",
            Token::Slash => "`/`",
            Token::LeftShift => "`<<`",
            Token::RightShift => "`>`",
            Token::Caret => "`^`",
            Token::And => "`&`",
            Token::Or => "`|`",
            Token::OpenAngle => "`<`",
            Token::CloseAngle => "`>`",
            Token::OpenSquare => "`[`",
            Token::CloseSquare => "`]`",
            Token::OpenParen => "`(`",
            Token::CloseParen => "`)`",
            Token::OpenBrace => "`{`",
            Token::CloseBrace => "`}`",
            Token::Arrow => "`->`",
            Token::Bang => "`!`",
            Token::Comma => "`,`",
            Token::Semicolon => "`;`",
            Token::Colon => "`:`",
            Token::ColonColon => "`::`",
            Token::Err => "lex error",
        }
    }

    pub fn from_ident(s: &'s str) -> Self {
        match s {
            "use" => Token::Use,
            "type" => Token::Type,
            "const" => Token::Const,
            "mut" => Token::Mut,
            "handle" => Token::Handle,
            "shared_handle" => Token::SharedHandle,
            "struct" => Token::Struct,
            "union" => Token::Union,
            "fn" => Token::Fn,
            "enum" => Token::Enum,
            s => Token::Identifier(s),
        }
    }
}

pub fn next_line<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> Skip {
    lexer.extras.cur_line += 1;
    lexer.extras.line_span_start = lexer.span().end;
    lexer.extras.linemap.push(lexer.extras.line_span_start);

    Skip
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

fn default_error<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> LexError<'src> {
    invalid_token(lexer).unwrap_err()
}

fn invalid_token<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> Result<(), LexError<'src>> {
    let tok = lexer.slice();
    let Span { start: begin, end } = lexer.span();

    let line = lexer.extras.cur_line;

    let begincol = (begin - lexer.extras.line_span_start) as u32;

    let begin_pos = Pos {
        row: line,
        col: begincol,
        byte_pos: begin,
    };

    let slide = end - begin;

    let end_col = begincol + (slide as u32);

    let end_pos = Pos {
        row: line,
        col: end_col,
        byte_pos: begin + slide,
    };
    Err(LexError {
        span: TextSpan {
            begin: begin_pos,
            end: end_pos,
        },
        token: Cow::Borrowed(tok),
        error_kind: LexErrorKind::UnknownToken,
    })
}

fn text_span_of<'src>(lexer: &Lexer<'src, Token<'src>>) -> TextSpan {
    let Span { start, end } = lexer.span();

    let line = lexer.extras.cur_line;
    let begin_col = (start - lexer.extras.line_span_start) as u32;
    let end_col = (end - lexer.extras.line_span_start) as u32;

    TextSpan {
        begin: Pos {
            row: line,
            col: begin_col,
            byte_pos: start,
        },
        end: Pos {
            row: line,
            col: end_col,
            byte_pos: end,
        },
    }
}

fn parse_uuid<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> Result<Uuid, LexError<'src>> {
    lilium_sys::uuid::try_parse_uuid(&lexer.slice()[1..])
        .map_err(|e| LexError {
            span: text_span_of(lexer),
            token: Cow::Borrowed(lexer.slice()),
            error_kind: LexErrorKind::InvalidUuid(e),
        })
        .map(Uuid)
}

pub use logos::Span;

pub fn lex_file<'src>(file: &'src str) -> Result<Vec<Spanned<Token<'src>>>, LexError<'src>> {
    let mut lexer = Token::lexer(file).spanned();

    let mut toks = Vec::new();

    while let Some((tok, span)) = lexer.next() {
        let tok = tok?;

        let Span { start, end } = span;

        let start_line = lexer
            .extras
            .linemap
            .binary_search(&start)
            .unwrap_or_else(|v| v - 1);

        let end_line = lexer
            .extras
            .linemap
            .binary_search(&end)
            .unwrap_or_else(|v| v - 1);

        let start_col = (start - lexer.extras.linemap[start_line]) as u32;
        let end_col = (end - lexer.extras.linemap[end_line]) as u32;

        let begin = Pos {
            row: start_line as u32,
            col: start_col,
            byte_pos: start,
        };
        let end = Pos {
            row: end_line as u32,
            col: end_col,
            byte_pos: end,
        };

        toks.push(Spanned(tok, TextSpan { begin, end }));
    }

    Ok(toks)
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Spanned<T>(pub T, pub TextSpan);

impl<T: core::fmt::Debug> core::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)?;
        f.write_str(" ")?;
        self.1.fmt(f)
    }
}
