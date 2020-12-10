use crate::elementrefs::{ClassRef, MethodType};
use crate::implm::consts::vtypes::*;
use std::hash::Hash;
use std::fmt::Debug;
use bytes::{BytesMut, BufMut};
pub use JIntegerStackType::*;
pub use JNumericStackType::{Float, Double};
pub use JNumericType::{Byte, Short, Char};
pub use JStackType::Reference;
pub use JObjectType::Class;
pub use JVerificationType::{Null, Uninitialized, UninitializedThis, Top};

macro_rules! cursedmacro {
	($slf:ident $($typ:ident($($entry:tt)+))+) => {
		$(
		impl From<$slf> for $typ {
			//type Target = $typ;
			fn from(other: $slf) -> $typ {
				match other {
					$($slf::$entry => $typ::$entry),+
				}
			}
		})+
	}
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum JType {
	Byte,
	Short,
	Int,
	Long,
	Float,
	Double,
	Boolean,
	Char,
	Class(ClassRef),
	Array(Box<JType>)
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum JObjectType {
	Class(ClassRef),
	Array(JType)
}

impl From<JObjectType> for JType {
	fn from(other: JObjectType) -> JType {
		match other {
			JObjectType::Class(r) => JType::Class(r),
			JObjectType::Array(b) => JType::Array(Box::new(b))
		}
	}
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Array(pub JType);
impl From<Array> for JType {
	fn from(a: Array) -> Self {
		Self::Array(Box::new(a.0))
	}
}
impl From<Array> for JObjectType {
	fn from(a: Array) -> Self {
		Self::Array(a.0)
	}
}
impl From<Array> for JVerificationType {
	fn from(a: Array) -> Self {
		Self::Class(a.into())
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum JBasicType {
	Byte,
	Short,
	Int,
	Long,
	Float,
	Double,
	Char,
	Boolean,
	Reference
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum JVerificationType {
	Top,
	Int,
	Float,
	Long,
	Double,
	Null,
	Class(JObjectType),
	ClassInternal(u16), // DO NOT USE, HACK FOR INTERNAL SERIALIZATION!!!!
	UninitializedThis,
	Uninitialized(u16),
}

impl JVerificationType {
	pub fn serialize(&self, buf: &mut BytesMut) {
		match self {
			Self::Top => buf.put_u8(V_TOP),
			Self::Int => buf.put_u8(V_INTEGER),
			Self::Float => buf.put_u8(V_FLOAT),
			Self::Long => buf.put_u8(V_LONG),
			Self::Double => buf.put_u8(V_DOUBLE),
			Self::Null => buf.put_u8(V_NULL),
			Self::UninitializedThis => buf.put_u8(V_UNINITIALIZED_THIS),
			Self::Class(t) => panic!("Cannot serialize class verification type directly"),
			Self::Uninitialized(o) => {buf.put_u8(V_UNINITIALIZED); buf.put_u16(*o)}
			Self::ClassInternal(p) => {buf.put_u8(V_OBJECT); buf.put_u16(*p)}
		}
	}
	
	pub fn is_wide(&self) -> bool {
		match self {
			Self::Long | Self::Double => true,
			_ => false
		}
	}
}

impl From<JObjectType> for JVerificationType {
	fn from(other: JObjectType) -> JVerificationType {
		Self::Class(other)
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum JStackType {
	Int,
	Long,
	Float,
	Double,
	Reference
}

cursedmacro! {
	JStackType
	JBasicType(Int Long Float Double Reference)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum JNumericType {
	Byte,
	Short,
	Int,
	Long,
	Float,
	Double,
	Char
}

cursedmacro! {
	JNumericType
	JType(Byte Short Int Long Float Double Char)
	JBasicType(Byte Short Int Long Float Double Char)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum JNumericStackType {
	Int,
	Long,
	Float,
	Double
}

cursedmacro! {
	JNumericStackType
	JNumericType(Int Long Float Double)
	JType(Int Long Float Double)
	JBasicType(Int Long Float Double)
	JVerificationType(Int Long Float Double)
	JStackType(Int Long Float Double)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum JIntegerStackType {
	Int,
	Long
}

cursedmacro! {
	JIntegerStackType
	JNumericType(Int Long)
	JNumericStackType(Int Long)
	JStackType(Int Long)
	JType(Int Long)
	JBasicType(Int Long)
	JVerificationType(Int Long)
}


impl JType {
	pub fn descriptor(&self) -> String {
		match self {
			Self::Byte => "B".into(),
			Self::Short => "S".into(),
			Self::Int => "I".into(),
			Self::Long => "J".into(),
			Self::Float => "F".into(),
			Self::Double => "D".into(),
			Self::Boolean => "Z".into(),
			Self::Char => "C".into(),
			Self::Class(c) => format!("L{};", &c[..]),
			Self::Array(c) => format!("[{}", &c.descriptor()[..])
		}
	}
	
	pub fn get_verification_type(&self) -> JVerificationType {
		match self {
			JType::Boolean | JType::Byte | JType::Short | JType::Char | JType::Int => JVerificationType::Int,
			JType::Float => JVerificationType::Float,
			JType::Long => JVerificationType::Long,
			JType::Double => JVerificationType::Double,
			JType::Class(c) => JVerificationType::Class(JObjectType::Class(c.clone())),
			JType::Array(a) => JVerificationType::Class(JObjectType::Array((**a).clone())),
		}
	}
}

pub enum Descriptor {
	Method(MethodType),
	Field(JType)
}
