use std::mem::replace;

use ::imt::{
    attr::{
        Attribute,
        types::{
            Align, DefinesBuiltinTypes, ExportInline, ItemDoc, OptionType, PolymorphicOption,
            Synthetic, SystemFunction,
        },
    },
    file, tydef,
    uses::{self as imt, BinaryOp, IntBits, IntType, UnaryOp},
    uuid::Uuid,
    value,
};

use crate::ast::{self, Generics, ItemBody, StructKind};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TyCheckError {
    WrongType(&'static str),
    OutOfRange(u128, IntType),
    AbsoluteConstantRequired,
    BadIntWidth(IntBits),
    WrongSuffixKind,
    NotPermitted(&'static str, StructKind),
}

pub fn fold_int(expr: &mut ast::Expr<'_>, ity: IntType) -> Result<Option<u128>, TyCheckError> {
    match expr {
        ast::Expr::IntLiteral(lit) => Ok(Some(*lit)),
        ast::Expr::StringLiteral(_) => Err(TyCheckError::WrongType("String")),
        ast::Expr::UuidLiteral(_) => Err(TyCheckError::WrongType("Uuid")),
        ast::Expr::IsInt(inner) => {
            let res = fold_int(inner, ity)?;
            let inner = replace((&mut **inner), ast::Expr::IntLiteral(0));
            *expr = inner;
            Ok(res)
        }
        ast::Expr::Named(_) => Ok(None),
        ast::Expr::BinaryOp(op, l, r) => {
            let l = fold_int(l, ity)?;
            let r = fold_int(r, ity)?;

            let Some((l, r)) = l.zip(r) else {
                return Ok(None);
            };

            let res = match op {
                BinaryOp::Add => l.wrapping_add(r),
                BinaryOp::Sub => l.wrapping_sub(r),
                BinaryOp::And => l & r,
                BinaryOp::Or => l | r,
                BinaryOp::Xor => l ^ r,
                BinaryOp::Mul => {
                    if ity.signed {
                        let l = l as i128;
                        let r = r as i128;
                        l.wrapping_mul(r) as u128
                    } else {
                        l.wrapping_mul(r)
                    }
                }
                BinaryOp::Div => {
                    if ity.signed {
                        let l = l as i128;
                        let r = r as i128;
                        l.wrapping_div(r) as u128
                    } else {
                        l / r
                    }
                }
                BinaryOp::ShiftLeft => l.wrapping_shl(r as u32),
                BinaryOp::ShiftRight => l.wrapping_shr(r as u32),
            };

            *expr = ast::Expr::IntLiteral(res);
            Ok(Some(res))
        }
        ast::Expr::UnaryOp(UnaryOp::Neg, inner) => {
            let Some(v) = fold_int(inner, ity)? else {
                return Ok(None);
            };

            let res = (!v).wrapping_add(1);
            *expr = ast::Expr::IntLiteral(res);
            Ok(Some(res))
        }
        ast::Expr::UnaryOp(UnaryOp::Not, inner) => {
            let Some(v) = fold_int(inner, ity)? else {
                return Ok(None);
            };
            let res = !v;
            *expr = ast::Expr::IntLiteral(res);
            Ok(Some(res))
        }
    }
}

pub fn convert_int(mut expr: ast::Expr<'_>, ty: IntType) -> Result<imt::Expr, TyCheckError> {
    fold_int(&mut expr, ty)?;

    match expr {
        ast::Expr::StringLiteral(_) | ast::Expr::UuidLiteral(_) | ast::Expr::IsInt(_) => {
            unreachable!()
        }
        ast::Expr::Named("__SIZEOF_POINTER__") => {
            Ok(imt::Expr::SpecialConstant(imt::SpecialConst::SizeofPointer))
        }
        ast::Expr::Named(id) => Ok(imt::Expr::Const(id.to_string())),
        ast::Expr::IntLiteral(lit) => Ok(imt::Expr::IntLiteral(ty, lit)),
        ast::Expr::BinaryOp(op, l, r) => Ok(imt::Expr::BinOp(
            op,
            Box::new(convert_int(*l, ty)?),
            Box::new(convert_int(*r, ty)?),
        )),
        ast::Expr::UnaryOp(op, val) => Ok(imt::Expr::UnaryOp(op, Box::new(convert_int(*val, ty)?))),
    }
}

fn unescape_literal(lit: &str) -> String {
    let mut lit = lit.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
    let mut collector = String::new();

    while let Some(idx) = lit.find('\\') {
        let (l, r) = lit.split_at(idx);
        collector.push_str(l);

        if let Some(s) = r.strip_prefix('n') {
            collector.push('\n');
            lit = s;
        } else if let Some(s) = r.strip_prefix('r') {
            collector.push('\r');
            lit = s;
        } else if let Some(s) = r.strip_prefix('"') {
            collector.push('"');
            lit = s;
        } else if let Some(s) = r.strip_prefix('\\') {
            collector.push('\\');
            lit = s;
        } else {
            panic!("Unknown escape sequence in literal");
        }
    }
    collector.push_str(lit);
    collector
}

pub fn convert_non_int(expr: ast::Expr<'_>) -> Result<imt::Expr, TyCheckError> {
    match expr {
        ast::Expr::StringLiteral(lit) => Ok(imt::Expr::StringLiteral(unescape_literal(lit))),
        ast::Expr::UuidLiteral(id) => Ok(imt::Expr::UuidLiteral(id)),
        ast::Expr::Named("__SIZEOF_POINTER__") => {
            Ok(imt::Expr::SpecialConstant(imt::SpecialConst::SizeofPointer))
        }
        ast::Expr::Named(id) => Ok(imt::Expr::Const(id.to_string())),
        _ => Err(TyCheckError::WrongType("int")),
    }
}

pub fn convert_type(ty: ast::Type, ctx_generics: &Generics) -> Result<imt::Type, TyCheckError> {
    match ty {
        ast::Type::Char => Ok(imt::Type::Char(IntType::u8)),
        ast::Type::Void => Ok(imt::Type::Void),
        ast::Type::Never => Ok(imt::Type::Void),
        ast::Type::Byte => Ok(imt::Type::Byte),
        ast::Type::IntType(ty) => {
            match ty.bits {
                IntBits::Bits(b) if matches!(b.get(), 8 | 16 | 32 | 64 | 128) => {}
                IntBits::Long => {}
                bits => return Err(TyCheckError::BadIntWidth(bits)),
            }

            Ok(imt::Type::Int(ty))
        }
        ast::Type::Named(n) => {
            if let Some((idx, _)) = ctx_generics
                .arg_names
                .iter()
                .copied()
                .enumerate()
                .find(|(_, s)| s == &n.base_name)
            {
                let suffix = match n.suffix {
                    Some(ast::GenericSuffix::Alternate(alt)) => {
                        Some(convert_type(*alt, ctx_generics)?)
                    }
                    Some(ast::GenericSuffix::Generics(_)) => {
                        return Err(TyCheckError::WrongSuffixKind);
                    }
                    None => None,
                };

                Ok(imt::Type::Param(idx as u32, suffix.map(Box::new)))
            } else {
                let args = match n.suffix {
                    Some(ast::GenericSuffix::Generics(gn)) => Some(
                        gn.into_iter()
                            .map(|ty| convert_type(ty, ctx_generics))
                            .collect::<Result<Vec<_>, _>>()?,
                    ),
                    Some(ast::GenericSuffix::Alternate(_)) => {
                        return Err(TyCheckError::WrongSuffixKind);
                    }
                    None => None,
                };

                Ok(imt::Type::Named(n.base_name.to_string(), args))
            }
        }
        ast::Type::Pointer(ptr) => {
            let kind = match ptr.pointer_kind {
                ast::PointerKind::Const => imt::PointerKind::Const,
                ast::PointerKind::Mut => imt::PointerKind::Mut,
                ast::PointerKind::Handle => {
                    imt::PointerKind::Special(Uuid::parse("aebb2f51-578f-562a-a9e8-d5afe8676cc2"))
                }
                ast::PointerKind::SharedHandle => {
                    imt::PointerKind::Special(Uuid::parse("5bcea25f-b255-5384-8357-d05c39936843"))
                }
            };

            let underlying = convert_type(*ptr.underlying, ctx_generics)?;

            Ok(imt::Type::Pointer(kind, Box::new(underlying)))
        }
        ast::Type::Array(arr) => {
            let elem = convert_type(*arr.elem, ctx_generics)?;
            let len = convert_int(arr.count, IntType::ulong)?;

            Ok(imt::Type::Array(Box::new(imt::ArrayType {
                base: elem,
                len,
            })))
        }
        ast::Type::FnPointer(sig) => {
            let sig = convert_signature(sig, ctx_generics)?;
            Ok(imt::Type::Func(sig))
        }
    }
}

pub fn convert_signature(
    sig: ast::Signature,
    ctx_generics: &Generics,
) -> Result<imt::Signature, TyCheckError> {
    let params = sig
        .params
        .into_iter()
        .map(|ast::Param { name, ty }| {
            Ok(imt::Param {
                attrs: Vec::new(),
                name: name.map(str::to_string),
                ty: convert_type(ty, ctx_generics)?,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let retty = convert_type(*sig.retty, ctx_generics)?;

    Ok(imt::Signature {
        params,
        retty: Box::new(retty),
    })
}

const EMPTY_ARGS: Generics = Generics {
    arg_names: Vec::new(),
};

pub fn convert_const_item(
    item: ast::ItemConst,
    doc_attr: ItemDoc,
) -> Result<value::Value, TyCheckError> {
    let ty = convert_type(item.ty, &EMPTY_ARGS)?;
    let val = match ty {
        imt::Type::Int(intty) => convert_int(item.value, intty)?,
        _ => convert_non_int(item.value)?,
    };

    let cn = value::Const {
        attrs: vec![Attribute::new(doc_attr)],
        ty,
        val,
    };

    Ok(value::Value {
        name: item.name.to_string(),
        body: value::ValueBody::Const(cn),
    })
}

pub fn convert_func_item(
    item: ast::ItemFunc,
    doc_attr: ItemDoc,
) -> Result<value::Value, TyCheckError> {
    let sysno = item
        .sysno
        .map(|mut v| fold_int(&mut v, IntType::u16))
        .transpose()?
        .flatten();
    let sig = convert_signature(item.sig, &EMPTY_ARGS)?;
    let mut attrs = Vec::new();

    attrs.push(Attribute::new(doc_attr));
    if let Some(sysno) = sysno {
        attrs.push(Attribute::new(SystemFunction {
            function_id: sysno
                .try_into()
                .map_err(|_| TyCheckError::OutOfRange(sysno, IntType::u16))?,
        }));
    }

    let func = value::Function {
        attrs,
        signature: sig,
    };

    Ok(value::Value {
        name: item.name.to_string(),
        body: value::ValueBody::Function(func),
    })
}

pub fn convert_field(
    field: ast::Field,
    generics: &ast::Generics,
) -> Result<tydef::Field, TyCheckError> {
    let ty = convert_type(field.ty, generics)?;

    Ok(tydef::Field {
        attrs: Vec::new(),
        name: field.name.to_string(),
        ty,
    })
}

pub fn convert_item(file: &mut file::File, item: ast::Item) -> Result<(), TyCheckError> {
    let doc_attr = ItemDoc {
        doc_lines: item.item_doc.into_iter().map(|v| v.to_string()).collect(),
    };
    match item.body {
        ast::ItemBody::Directive("%def_handle_type") => {
            file.attributes
                .push(Attribute::new(DefinesBuiltinTypes::Handle));
            Ok(())
        }
        ast::ItemBody::Directive("%def_sys_result2") => {
            file.attributes
                .push(Attribute::new(DefinesBuiltinTypes::SysResult2));
            Ok(())
        }
        ast::ItemBody::Directive("%def_int_types") => Ok(()),
        ast::ItemBody::Directive(x) => todo!("{x}"),
        ast::ItemBody::Use(item_use) => {
            let path = item_use.path.into_iter().map(|v| v.to_string()).collect();

            let mut imt_use = file::UseItem {
                attrs: Vec::new(),
                path,
            };

            if item_use.inline {
                imt_use.attrs.push(Attribute::new(ExportInline));
            }

            file.uses.push(imt_use);
            Ok(())
        }
        ast::ItemBody::TypeAlias(type_alias) => {
            let generics = type_alias
                .generics
                .as_ref()
                .unwrap_or(const { &EMPTY_ARGS });

            let num_params = generics.arg_names.len() as u32;
            let ty = convert_type(type_alias.def, generics)?;
            let mut alias = tydef::TypeAlias {
                attrs: vec![],
                alias: ty,
            };
            alias.attrs.push(Attribute::new(doc_attr));
            file.types.push(tydef::TypeDef {
                name: type_alias.name.to_string(),
                num_params,
                body: tydef::TypeDefBody::Alias(alias),
            });
            Ok(())
        }
        ast::ItemBody::Struct(item_struct) => {
            let generics = item_struct
                .generics
                .as_ref()
                .unwrap_or(const { &EMPTY_ARGS });

            let param_count = generics.arg_names.len();

            let body = match item_struct.body {
                ast::StructBody::Fields(fields) => {
                    let fields = tydef::StructFields {
                        field: fields
                            .fields
                            .into_iter()
                            .map(|f| convert_field(f, generics))
                            .collect::<Result<Vec<_>, _>>()?,
                        pad: fields
                            .pad
                            .map(|ty| convert_type(ty, generics))
                            .transpose()?,
                    };

                    match item_struct.kind {
                        ast::StructKind::Struct => {
                            let body = tydef::StructBody::Fields(fields);
                            let mut st = tydef::Struct {
                                attrs: Vec::new(),
                                body,
                            };

                            st.attrs.push(Attribute::new(doc_attr));

                            for attr in item_struct.attrs {
                                match attr {
                                    ast::StructAttribute::Align(mut expr) => {
                                        let alignment = fold_int(&mut expr, IntType::ulong)?
                                            .ok_or_else(|| {
                                                TyCheckError::AbsoluteConstantRequired
                                            })?;
                                        st.attrs.push(Attribute::new(Align { alignment }))
                                    }
                                    ast::StructAttribute::Option(expr) => {
                                        if let Some(expr) = expr {
                                            let expr = convert_non_int(expr)?;
                                            let id = match expr {
                                                imt::Expr::UuidLiteral(lit) => lit,
                                                imt::Expr::Const(_)
                                                | imt::Expr::SpecialConstant(_) => {
                                                    return Err(
                                                        TyCheckError::AbsoluteConstantRequired,
                                                    );
                                                }
                                                _ => return Err(TyCheckError::WrongType("Uuid")),
                                            };
                                            st.attrs.push(Attribute::new(OptionType { option: id }))
                                        } else {
                                            st.attrs.push(Attribute::new(PolymorphicOption))
                                        }
                                    }
                                    ast::StructAttribute::OptionHead(_) => {
                                        return Err(TyCheckError::NotPermitted(
                                            "option_head",
                                            ast::StructKind::Struct,
                                        ));
                                    }
                                }
                            }

                            tydef::TypeDefBody::Struct(st)
                        }
                        ast::StructKind::Union => {
                            let mut un = tydef::Union {
                                attrs: Vec::new(),
                                fields,
                            };
                            un.attrs.push(Attribute::new(doc_attr));

                            for attr in item_struct.attrs {
                                match attr {
                                    ast::StructAttribute::Align(mut expr) => {
                                        let alignment = fold_int(&mut expr, IntType::ulong)?
                                            .ok_or_else(|| {
                                                TyCheckError::AbsoluteConstantRequired
                                            })?;
                                        un.attrs.push(Attribute::new(Align { alignment }))
                                    }
                                    ast::StructAttribute::Option(_) => {
                                        return Err(TyCheckError::NotPermitted(
                                            "option",
                                            ast::StructKind::Union,
                                        ));
                                    }
                                    ast::StructAttribute::OptionHead(sz) => {
                                        let mut unknown_name = item_struct.name.to_string();
                                        unknown_name.push_str("_Unknown");
                                        let sz = convert_int(sz, IntType::ulong)?;
                                        un.fields.field.push(tydef::Field {
                                            attrs: vec![Attribute::new(Synthetic)],
                                            name: format!("unknown"),
                                            ty: imt::Type::Named(unknown_name.clone(), None),
                                        });

                                        let unknown_body = tydef::StructFields {
                                            field: vec![tydef::Field {
                                                attrs: vec![Attribute::new(Synthetic)],
                                                name: format!("pad"),
                                                ty: imt::Type::Array(Box::new(imt::ArrayType {
                                                    base: imt::Type::Byte,
                                                    len: sz,
                                                })),
                                            }],
                                            pad: None,
                                        };

                                        let unknown = tydef::Struct {
                                            attrs: vec![
                                                Attribute::new(Synthetic),
                                                Attribute::new(PolymorphicOption),
                                            ],
                                            body: tydef::StructBody::Fields(unknown_body),
                                        };
                                        file.types.push(tydef::TypeDef {
                                            name: unknown_name,
                                            num_params: 0,
                                            body: tydef::TypeDefBody::Struct(unknown),
                                        })
                                    }
                                }
                            }
                            tydef::TypeDefBody::Union(un)
                        }
                    }
                }
                ast::StructBody::Opaque(ty) => {
                    if item_struct.kind == StructKind::Union {
                        return Err(TyCheckError::NotPermitted("opaque", StructKind::Union));
                    }

                    let mut st = tydef::Struct {
                        attrs: vec![],
                        body: tydef::StructBody::Opaque(
                            ty.map(|ty| convert_type(ty, generics)).transpose()?,
                        ),
                    };
                    st.attrs.push(Attribute::new(doc_attr));
                    tydef::TypeDefBody::Struct(st)
                }
            };

            let item = tydef::TypeDef {
                name: item_struct.name.to_string(),
                num_params: param_count as u32,
                body,
            };
            file.types.push(item);
            Ok(())
        }
        ast::ItemBody::Enum(item_enum) => todo!("enum"),
        ast::ItemBody::Const(item) => Ok(file.values.push(convert_const_item(item, doc_attr)?)),
        ast::ItemBody::Func(item) => Ok(file.values.push(convert_func_item(item, doc_attr)?)),
    }
}
