use imt::{uses::IntType, uuid::Uuid};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct File<'src> {
    pub file_doc: Vec<&'src str>,
    pub items: Vec<Item<'src>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Item<'src> {
    pub item_doc: Vec<&'src str>,
    pub body: ItemBody<'src>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ItemBody<'src> {
    Directive(&'src str),
    Use(ItemUse<'src>),
    TypeAlias(TypeAlias<'src>),
    Struct(ItemStruct<'src>),
    Enum(ItemEnum<'src>),
    Const(ItemConst<'src>),
    Func(ItemFunc<'src>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ItemUse<'src> {
    pub inline: bool,
    pub path: Vec<&'src str>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeAlias<'src> {
    pub name: &'src str,
    pub generics: Option<Generics<'src>>,
    pub def: Type<'src>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Generics<'src> {
    pub arg_names: Vec<&'src str>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ItemStruct<'src> {
    pub kind: StructKind,
    pub name: &'src str,
    pub generics: Option<Generics<'src>>,
    pub attrs: Vec<StructAttribute<'src>>,
    pub body: StructBody<'src>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StructKind {
    Struct,
    Union,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum StructAttribute<'src> {
    Align(Expr<'src>),
    Option(Option<Expr<'src>>),
    OptionHead(Expr<'src>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum StructBody<'src> {
    Fields(Fields<'src>),
    Opaque(Option<Type<'src>>),
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Fields<'src> {
    pub fields: Vec<Field<'src>>,
    pub pad: Option<Type<'src>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Field<'src> {
    pub name: &'src str,
    pub ty: Type<'src>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ItemEnum<'src> {
    pub name: &'src str,
    pub underlying: IntType,
    pub variants: Vec<Variant<'src>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Variant<'src> {
    pub name: &'src str,
    pub discriminant: Expr<'src>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ItemConst<'src> {
    pub name: &'src str,
    pub ty: Type<'src>,
    pub value: Expr<'src>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ItemFunc<'src> {
    pub name: &'src str,
    pub sig: Signature<'src>,
    pub sysno: Option<Expr<'src>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Signature<'src> {
    pub params: Vec<Param<'src>>,
    pub retty: Box<Type<'src>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Param<'src> {
    pub name: Option<&'src str>,
    pub ty: Type<'src>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type<'src> {
    IntType(IntType),
    Char,
    Void,
    Never,
    Byte,
    Named(NamedType<'src>),
    Array(ArrayType<'src>),
    Pointer(PointerType<'src>),
    FnPointer(Signature<'src>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NamedType<'src> {
    pub base_name: &'src str,
    pub suffix: Option<GenericSuffix<'src>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericSuffix<'src> {
    Generics(Vec<Type<'src>>),
    Alternate(Box<Type<'src>>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArrayType<'src> {
    pub elem: Box<Type<'src>>,
    pub count: Expr<'src>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PointerType<'src> {
    pub pointer_kind: PointerKind,
    pub underlying: Box<Type<'src>>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum PointerKind {
    Const,
    Mut,
    Handle,
    SharedHandle,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr<'src> {
    IntLiteral(u128),
    UuidLiteral(Uuid),
    StringLiteral(&'src str),
    Named(&'src str),
    BinaryOp(BinaryOp, Box<Expr<'src>>, Box<Expr<'src>>),
    UnaryOp(UnaryOp, Box<Expr<'src>>),
    IsInt(Box<Expr<'src>>),
}

pub use imt::uses::{BinaryOp, UnaryOp};
