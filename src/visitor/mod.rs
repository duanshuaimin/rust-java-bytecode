pub mod writer;
pub mod util;

use crate::{
	access::*,
	elementrefs::*,
	opcodes::Opcode,
	opcodes::Label,
	types::JType,
	types::JVerificationType
};
use std::cmp::Ordering;
use bytes::Bytes;

pub trait ClassVisitorFactory {
	type ClassVisitorType: ClassVisitor;
	fn create(&self, access: ClassAccess, name: String, superclass: ClassRef, interfaces: Vec<ClassRef>, major: u16, minor: u16) -> Self::ClassVisitorType;
}

pub trait ClassVisitor {
	type MethodVisitorType: MethodVisitor;
	type FieldVisitorType: FieldVisitor;
	fn visit_method(&mut self, access: MethodAccess, name: String, desc: MethodType) -> Option<Self::MethodVisitorType>;
	fn visit_field(&mut self, access: FieldAccess, name: String, fieldtype: JType) -> Option<Self::FieldVisitorType>;
}

pub trait MethodVisitor {
	type CodeVisitorType: CodeVisitor;
	fn visit_code(&mut self) -> Option<Self::CodeVisitorType>;
}

pub struct ExceptionHandler {
	pub start: Label,
	pub end: Label,
	pub handler: Label,
	pub exception: Option<ClassRef>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StackFrame {
	Same(Label),
	SameLocals1Stack(Label, JVerificationType),
	Chop(Label, u8),
	Append(Label, Vec<JVerificationType>),
	Full(Label, Vec<JVerificationType>, Vec<JVerificationType>)
}

impl StackFrame {
	fn get_label(&self) -> &Label {
		match self {
			Self::Same(l) => l,
			Self::SameLocals1Stack(l, _) => l,
			Self::Chop(l, _) => l,
			Self::Append(l, _) => l,
			Self::Full(l, _, _) => l
		}
	}
}

impl PartialOrd for StackFrame {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.get_label().info.get()?.offset.cmp(&other.get_label().info.get()?.offset))
	}
}

impl Ord for StackFrame {
    fn cmp(&self, other: &Self) -> Ordering {
        if let Some(o) = self.partial_cmp(other) {
			o
		} else {
			Ordering::Equal
		}
    }
}

pub trait CodeVisitor {
	type EndReturn;
	
	fn visit_opcode(&mut self, opcode: Opcode);
	fn visit_label(&mut self, label: Label);
	fn visit_maxs(&mut self, max_stack: u16, max_locals: u16);
	fn visit_exception_handler(&mut self, handler: ExceptionHandler);
	fn visit_frame(&mut self, frame: StackFrame);
	fn visit_end(&mut self) -> Self::EndReturn; // some CVs do computations here, return a Result
}

pub trait GetOffset {
	fn get_offset(&self) -> u16;
}

pub trait Serialize {
	fn serialize(&self) -> Bytes;
}

pub trait FieldVisitor {
	
}