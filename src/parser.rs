use imt::uses::IntType;
use peg::error::ParseError as PegParseError;

use crate::lexer::{Spanned, Token};

use crate::ast::*;

fn parse_int_radix(x: &str, radix: u128) -> u128 {
    let mut running = 0u128;
    for b in x.bytes() {
        if b == b'_' {
            continue;
        }
        let val = match b {
            b'0'..=b'9' => b - b'0',
            b'A'..=b'F' => b - b'A' + 10,
            b'a'..=b'f' => b - b'a' + 10,
            _ => unreachable!(),
        };

        running *= radix;
        running += val as u128;
    }
    running
}

fn parse_int(x: &str) -> u128 {
    if let Some(hex_literal) = x.strip_prefix("0x") {
        parse_int_radix(hex_literal, 16)
    } else if let Some(oct_literal) = x.strip_prefix("0o") {
        parse_int_radix(oct_literal, 8)
    } else {
        parse_int_radix(x, 10)
    }
}

fn binary_op<'src>(op: BinaryOp, l: Expr<'src>, r: Expr<'src>) -> Expr<'src> {
    Expr::BinaryOp(op, Box::new(l), Box::new(r))
}

use core::num::NonZero;

peg::parser! {
    grammar knums<'src>() for Tokens<'src> {
        rule simple_expr() -> Expr<'src> = [Token::OpenParen] e:expr() [Token::CloseParen] {
            e
        } / [Token::Identifier(id)] { Expr::Named(id) } / [Token::UuidLiteral(id)] {
            Expr::UuidLiteral(id)
        } / [Token::IntLiteral(lit)] {
            Expr::IntLiteral(parse_int(lit))
        } / [Token::StringLiteral(lit)] {
            Expr::StringLiteral(lit)
        }

        rule unary_expr() -> Expr<'src> = op:[Token::Minus | Token::Bang] e:unary_expr() {
            let op = match op {
                Token::Minus => UnaryOp::Neg,
                Token::Bang => UnaryOp::Not,
                _ => unreachable!()
            };

            Expr::UnaryOp(op, Box::new(e))
        } / op:[Token::Plus] e:unary_expr() {
            Expr::IsInt(Box::new(e))
        } / e:simple_expr() {
            e
        }

        rule binary_expr() -> Expr<'src> = precedence! {
            l:(@) [Token::Plus] r:@ {binary_op(BinaryOp::Add, l, r)}
            l:(@) [Token::Minus] r:@ {binary_op(BinaryOp::Sub, l, r)}
            --
            l:(@) [Token::Star] r:@ {binary_op(BinaryOp::Mul, l, r)}
            l:(@) [Token::Slash] r:@ {binary_op(BinaryOp::Div, l, r)}
            --
            l:(@) [Token::And] r:@ {binary_op(BinaryOp::And, l, r)}
            l:(@) [Token::Or] r:@ {binary_op(BinaryOp::Or, l, r)}
            l:(@) [Token::Caret] r:@ {binary_op(BinaryOp::Xor, l, r)}
            --
            l:(@) [Token::LeftShift] r:@ {binary_op(BinaryOp::ShiftLeft, l, r)}
            l:(@) [Token::RightShift] r:@ {binary_op(BinaryOp::ShiftRight, l, r)}
            --
            e:unary_expr() {e}
        }

        pub rule expr() -> Expr<'src> = e:binary_expr() {
            e
        }

        rule generic_params() -> Vec<Type<'src>> = e:(match_type() ** [Token::Comma]) [Token::Comma]? {
            e
        };

        rule named_type() -> NamedType<'src> = [Token::Identifier(id)]  suffix:(([Token::Bang] fallback:match_type() {
            GenericSuffix::Alternate(Box::new(fallback))
        } / [Token::OpenAngle] g:generic_params() [Token::CloseAngle] {
            GenericSuffix::Generics(g)
        })?){
            NamedType { base_name: id, suffix }
        }

        rule int_type() -> IntType = [Token::Identifier(id)] {?
            match id {
                "ilong" => Ok(IntType::ilong),
                "ulong" => Ok(IntType::ulong),
                x if x.starts_with("i") || x.starts_with("u") => {
                    let signed = x.starts_with("i");
                    let bits = x[1..].parse::<u8>().map_err(|_| "Expected an Integer type")?;
                    Ok(IntType{signed, bits: imt::uses::IntBits::Bits(NonZero::new(bits).ok_or("Expected an integer type")?)})
                }
                _ => Err("Expected an Integer type")
            }
        }

        rule special_named_type() -> Type<'src> = int:int_type() {Type::IntType(int)} / [Token::Identifier("void")] {Type::Void} / [Token::Identifier("char")] {Type::Char} / [Token::Identifier("byte")] {Type::Byte}

        pub rule match_type() -> Type<'src> = special_named_type() / named:named_type() {
            Type::Named(named)
        } / [Token::OpenSquare] ty:match_type() [Token::Semicolon] len:expr() [Token::CloseSquare] {
            Type::Array(ArrayType {
                elem: Box::new(ty),
                count: len,
            })
        } / [Token::Fn] sig:signature() {
            Type::FnPointer(sig)
        } / [Token::Bang] {
            Type::Never
        } / ptr:pointer_type() {Type::Pointer(ptr)}

        rule pointer_type() -> PointerType<'src> = [Token::Star] kind:([Token::Const] {PointerKind::Const} / [Token::Mut] {PointerKind::Mut} / [Token::Handle] {PointerKind::Handle} / [Token::SharedHandle] {PointerKind::SharedHandle}) underlying:match_type() {
            PointerType{pointer_kind: kind, underlying: Box::new(underlying)}
        }

        rule signature() -> Signature<'src> = [Token::OpenParen] params:(param() ** [Token::Comma]) [Token::Comma]? [Token::CloseParen] [Token::Arrow] retty:match_type() {
            Signature { params, retty: Box::new(retty) }
        }

        rule param() -> Param<'src> = name:([Token::Identifier(id)] [Token::Colon] {
            id
        })? ty:match_type() {
            Param { name, ty }
        }

        rule func() -> ItemFunc<'src> = [Token::Fn] [Token::Identifier(name)] sig:signature() sysno:([Token::Equals] e:expr() {
            e
        })? [Token::Semicolon] {
            ItemFunc { name, sig, sysno }
        }

        rule const_item() -> ItemConst<'src> = [Token::Const] [Token::Identifier(name)] [Token::Colon] ty:match_type() [Token::Equals] value:expr() [Token::Semicolon] {
            ItemConst { name, ty, value }
        }

        rule struct_attribute() -> StructAttribute<'src> = [Token::Identifier("align")] [Token::OpenParen] align:expr() [Token::CloseParen] {
            StructAttribute::Align(align)
        } / [Token::Identifier("option")] [Token::OpenParen] id:expr() [Token::CloseParen] {
            StructAttribute::Option(Some(id))
        } / [Token::Identifier("option")] {
            StructAttribute::Option(None)
        } / [Token::Identifier("option_head")] [Token::OpenParen] payload_size:expr() [Token::CloseParen] {
            StructAttribute::OptionHead(payload_size)
        }

        rule struct_attributes() -> Vec<StructAttribute<'src>> = attrs:([Token::Colon] attrs:struct_attribute()* {
            attrs
        })? {
            attrs.unwrap_or_else(Vec::new)
        }

        rule generics() -> Generics<'src> = [Token::OpenAngle] params:(([Token::Identifier(name)] { name })  ** [Token::Comma]) [Token::Comma]? [Token::CloseAngle] {
            Generics{arg_names: params}
        };

        rule field() -> Field<'src> = capture_pub() / doc:(([Token::Doc(doc)] {doc})*) [Token::Identifier(name)] [Token::Colon] ty:match_type() {
            Field{name, ty, doc}
        }

        rule fields_helper() -> (Vec<Field<'src>>, Option<Result<Field<'src>, Type<'src>>>) = fields:((field:field() [Token::Comma] { field })* ) tail:(last_field:field() {
            Ok(last_field)
        } / [Token::Identifier("pad")] [Token::OpenParen] ty:match_type() [Token::CloseParen] {
            Err(ty)
        })? {
            (fields, tail)
        }
        rule fields() -> Fields<'src> = fields:fields_helper() {
            let (mut fields, tail) = fields;

            match tail {
                Some(Ok(field)) => {fields.push(field); Fields{fields, pad: None}},
                Some(Err(pad)) => Fields{fields, pad: Some(pad)},
                None => Fields{fields, pad: None}
            }
        }

        rule struct_body() -> StructBody<'src> = [Token::OpenBrace] fields:fields() [Token::CloseBrace] {
            StructBody::Fields(fields)
        } / [Token::Identifier("opaque")] ty:([Token::OpenParen] ty:match_type() [Token::CloseParen] {
            ty
        })? [Token::Semicolon] {
            StructBody::Opaque(ty)
        }

        rule struct_item() -> ItemStruct<'src> = kind:([Token::Struct] {StructKind::Struct} / [Token::Union] {StructKind::Union}) [Token::Identifier(name)] generics:generics()? attrs:struct_attributes() body:struct_body() {
            ItemStruct {kind, name, generics, attrs, body}
        }

        rule variant() -> Variant<'src> = [Token::Identifier(name)] [Token::Equals] e:expr() {
            Variant {
                name,
                discriminant: e
            }
        }

        rule enum_item() -> ItemEnum<'src> = [Token::Enum] [Token::Identifier(name)] [Token::Colon] underlying:int_type() [Token::OpenBrace] variants:(variant:variant() ** [Token::Comma]) [Token::Comma]? [Token::CloseBrace] {
            ItemEnum {
                name,
                underlying,
                variants,
            }
        }

        rule type_alias() -> TypeAlias<'src> = [Token::Type] [Token::Identifier(name)] generics:generics()? [Token::Equals] ty:match_type() [Token::Semicolon] {
            TypeAlias {
                name,
                generics,
                def: ty
            }
        }

        rule use_item() -> ItemUse<'src>  = inline:("inline"?) "use" path:(([Token::Identifier(segment)] {segment}) ** [Token::ColonColon]) [Token::Semicolon] {
            ItemUse{inline: inline.is_some(), path}
        }

        rule capture_pub<T: 'src>() -> T = "pub" {?
            Err("Unexpected use of `pub`")
        }

        rule item_body() -> ItemBody<'src> = capture_pub() / func:func() {
            ItemBody::Func(func)
        } / cn:const_item() {
            ItemBody::Const(cn)
        } / st:struct_item() {
            ItemBody::Struct(st)
        } / en:enum_item() {
            ItemBody::Enum(en)
        } / ty:type_alias() {
            ItemBody::TypeAlias(ty)
        } /  us:use_item() {
            ItemBody::Use(us)
        }

        pub rule item() -> Item<'src> = [Token::Directive(dir)] {
            Item{item_doc: Vec::new(), body: ItemBody::Directive(dir.trim())}
        } / item_doc:(([Token::Doc(doc)] {doc})*) body:item_body() {
            Item{item_doc, body}
        }

        pub rule file() -> File<'src> = file_doc:(([Token::InnerDoc(doc)] {doc})*) items:item()* {
            File { file_doc, items }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError<'src> {
    pub underlying: PegParseError<usize>,
    pub got: Spanned<Token<'src>>,
}

impl<'src> core::fmt::Display for ParseError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Error at ")?;
        self.got.1.fmt(f)?;

        f.write_str(": expected one of ")?;

        self.underlying.expected.fmt(f)?;

        f.write_str(" got ")?;

        f.write_str(self.got.0.class())
    }
}

#[repr(transparent)]
pub struct Tokens<'src>([Spanned<Token<'src>>]);

impl<'src> peg::Parse for Tokens<'src> {
    type PositionRepr = usize;

    fn is_eof<'input>(&'input self, p: usize) -> bool {
        p == self.0.len()
    }

    fn position_repr<'input>(&'input self, p: usize) -> Self::PositionRepr {
        p
    }

    fn start<'input>(&'input self) -> usize {
        0
    }
}

impl<'src, 'input> peg::ParseElem<'input> for Tokens<'src> {
    type Element = Token<'src>;

    fn parse_elem(&'input self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self.0[pos..].first() {
            Some(elem) => peg::RuleResult::Matched(pos + 1, elem.0),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'src> peg::ParseLiteral for Tokens<'src> {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> peg::RuleResult<()> {
        match self.0[pos..].first() {
            Some(elem) if elem.0 == Token::from_ident(literal) => {
                peg::RuleResult::Matched(pos + 1, ())
            }
            _ => peg::RuleResult::Failed,
        }
    }
}

impl<'src: 'input, 'input> peg::ParseSlice<'input> for Tokens<'src> {
    type Slice = &'input Self;

    fn parse_slice(&'input self, p1: usize, p2: usize) -> &'input Self {
        unsafe { &*((&raw const self.0[p1..p2]) as *const Self) }
    }
}

pub fn parse_file<'src>(src: &'src [Spanned<Token<'src>>]) -> Result<File<'src>, ParseError<'src>> {
    // SAFETY: We never borrow a token for less than the lifetime of the whole array, which is 'src bounding lifetime
    knums::file(unsafe { &*(src as *const [_] as *const Tokens<'_>) }).map_err(|e| ParseError {
        got: src[e.location],
        underlying: e,
    })
}
