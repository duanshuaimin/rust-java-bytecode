use crate::types::JType;
use crate::implm::consts::handle::*;

pub type ClassRef = String;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct MethodType {
	args: Vec<JType>,
	ret: Option<JType>
}

impl MethodType {
	pub fn new(args: Vec<JType>, ret: Option<JType>) -> MethodType {
		MethodType {
			args, ret
		}
	}
	
	pub fn descriptor(&self) -> String {
		let mut s = String::new();
		for t in &self.args {
			s += &t.descriptor();
		}
		let ret = match &self.ret {
			Some(t) => t.descriptor(),
			None => "V".into()
		};
		format!("({}){}", s, ret)
	}
	
	pub fn get_args(&self) -> &Vec<JType> {
		&self.args
	}
	
	pub fn get_return(&self) -> &Option<JType> {
		&self.ret
	}
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct MethodRef {
	class: ClassRef,
	name: String,
	t: MethodType
}

impl MethodRef {
	pub const fn new(class: ClassRef, name: String, t: MethodType) -> MethodRef {
		MethodRef{class, name, t}
	}
	
	pub fn get_class(&self) -> &ClassRef {
		&self.class
	}
	
	pub fn get_name(&self) -> &String {
		&self.name
	}
	
	pub fn get_type(&self) -> &MethodType {
		&self.t
	}
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Handle {
	Method(MethodHandle),
	Field(FieldHandle)
}

impl Handle {
	pub fn get_index(&self) -> u8 {
		match self {
			Self::Method(mh) => mh.1.get_index(),
			Self::Field(fh) => fh.1.get_index()
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum FieldHandleAction {
	PutStatic,
	GetStatic,
	PutField,
	GetField
}

impl FieldHandleAction {
	pub fn get_index(&self) -> u8 {
		match self {
			Self::GetField => H_GETFIELD,
			Self::GetStatic => H_GETSTATIC,
			Self::PutField => H_PUTFIELD,
			Self::PutStatic => H_PUTSTATIC
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum MethodHandleAction {
	InvokeVirtual,
	InvokeStatic,
	InvokeSpecial,
	NewInvokeSpecial,
	InvokeInterface
}

impl MethodHandleAction {
	pub fn get_index(&self) -> u8 {
		match self {
			Self::InvokeVirtual => H_INVOKEVIRTUAL,
			Self::InvokeStatic => H_INVOKESTATIC,
			Self::InvokeSpecial => H_INVOKESPECIAL,
			Self::NewInvokeSpecial => H_NEWINVOKESPECIAL,
			Self::InvokeInterface => H_INVOKEINTERFACE
		}
	}
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct MethodHandle(pub MethodRef, pub MethodHandleAction);
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FieldHandle(pub FieldRef, pub FieldHandleAction);

#[derive(Clone, PartialEq, Debug)]
pub struct CallSite {
	pub bootstrap: MethodHandle,
	pub name: String,
	pub ty: MethodType,
	pub args: Vec<CallSiteConstant>
}

#[derive(Clone, PartialEq, Debug)]
pub enum CallSiteConstant {
	Int(i32),
	Long(i64),
	Float(f32),
	Double(f64),
	Handle(Handle),
	Str(String),
	Class(ClassRef)
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FieldRef {
	class: ClassRef,
	name: String,
	jtype: JType
}

impl FieldRef {
	pub fn get_class(&self) -> &ClassRef {
		&self.class
	}
	
	pub fn get_name(&self) -> &String {
		&self.name
	}
	
	pub fn get_type(&self) -> &JType {
		&self.jtype
	}
}

impl FieldRef {
	pub const fn new(class: ClassRef, name: String, jtype: JType) -> FieldRef {
		FieldRef{class, name, jtype}
	}
}