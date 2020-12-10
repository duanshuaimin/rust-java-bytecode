use crate::{
	types::*,
	elementrefs::*
};
use std::cell::Cell;
use std::rc::Rc;
pub use Opcode::*;

#[derive(PartialEq, Clone, Debug)]
pub enum JConst {
	Byte(i8),
	Short(i16),
	Int(i32),
	Long(i64),
	Float(f32),
	Double(f64),
	Boolean(bool),
	Char(char),
	Class(ClassRef),
	Handle(Handle),
	MethodType(MethodType),
	Str(String),
	Null,
}

impl JConst {
	pub fn get_verification_type(&self) -> JVerificationType {
		match self {
			JConst::Byte(_) | JConst::Short(_) | JConst::Char(_) | JConst::Int(_) | JConst::Boolean(_) => Int.into(),
			JConst::Float(_) => Float.into(),
			JConst::Long(_) => Long.into(),
			JConst::Double(_) => Double.into(),
			JConst::Class(_) => Class("java/lang/Class".into()).into(),
			JConst::Handle(_) => Class("java/lang/invoke/MethodHandle".into()).into(),
			JConst::MethodType(_) => Class("java/lang/invoke/MethodType".into()).into(),
			JConst::Str(_) => Class("java/lang/String".into()).into(),
			JConst::Null => Null
		}
	}
	
	pub fn is_wide(&self) -> bool {
		match self {
			JConst::Long(_) | JConst::Double(_) => true,
			_ => false
		}
	}
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct Label {
	pub info: Rc<Cell<Option<LabelInfo>>>
}

#[derive(Clone, Default, Debug, Copy, PartialEq, Eq)]
pub struct LabelInfo {
	pub offset: u16,
	pub opcode_offset: u16
}

#[derive(Clone, Default, Debug, Copy)]
pub struct LabelData {
	pub offset: u16,
	pub align: bool
}

impl Label {
	pub fn new() -> Self {
		Label {
			info: Rc::new(Cell::new(None))
		}
	}
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ShiftType {
	SignedLeft,
	SignedRight,
	UnsignedRight
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum RefComparison {
	Equal,
	NotEqual
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum Comparison {
	Equal,
	NotEqual,
	LessThan,
	GreaterThan,
	LessOrEqual,
	GreaterOrEqual
}

impl From<RefComparison> for Comparison {
	fn from(other: RefComparison) -> Comparison {
		match other {
			RefComparison::Equal => Self::Equal,
			RefComparison::NotEqual => Self::NotEqual
		}
	}
}

#[derive(Clone, Debug)]
pub enum Opcode {
	Nop,
	Const(JConst),
	Load(JStackType, u16),
	ArrayLoad(JBasicType),
	Store(JStackType, u16),
	ArrayStore(JBasicType),
	Dup,
	DupX1,
	DupX2,
	Dup2,
	Dup2X1,
	Dup2X2,
	Pop,
	Pop2,
	Swap,
	Add(JNumericStackType),
	Sub(JNumericStackType),
	Mul(JNumericStackType),
	Div(JNumericStackType),
	Rem(JNumericStackType),
	Neg(JNumericStackType),
	IInc(u16, i16),
	NumConvert(JNumericStackType, JNumericStackType),
	IConvert(JNumericType),
	LCmp,
	FCmpL,
	FCmpG,
	DCmpL,
	DCmpG,
	If(Comparison, Label),
	IfICmp(Comparison, Label),
	IfACmp(RefComparison, Label),
	Goto(Label),
	JSr(Label),
	Ret(u16),
	Return(Option<JStackType>),
	Shift(JIntegerStackType, ShiftType),
	And(JIntegerStackType),
	Or(JIntegerStackType),
	Xor(JIntegerStackType),
	GetStatic(FieldRef),
	PutStatic(FieldRef),
	GetField(FieldRef),
	PutField(FieldRef),
	InvokeVirtual(MethodRef, bool),
	InvokeSpecial(MethodRef, bool),
	InvokeStatic(MethodRef, bool),
	InvokeInterface(MethodRef),
	InvokeDynamic(CallSite),
	New(ClassRef),
	NewArray(JType),
	MultiANewArray(Array, u8),
	ArrayLength,
	AThrow,
	CheckCast(ClassRef),
	InstanceOf(ClassRef),
	MonitorEnter,
	MonitorExit,
	IfNull(Label),
	IfNonNull(Label),
	LookupSwitch(Label, Vec<(i32, Label)>),
	TableSwitch(Label, i32, i32, Vec<Label>)
}