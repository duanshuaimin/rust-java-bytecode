pub mod writer;
pub mod util;

use crate::{
	access::*,
	elementrefs::*,
	opcodes::Opcode,
	opcodes::Label,
	types::JType,
	types::JVerificationType,
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
	fn visit_signature(&mut self, sig: String);
	fn visit_inner_classes(&mut self, classes: Vec<InnerClassInfo>);
	fn visit_enclosing_method(&mut self, outer_class: ClassRef, outer_method: Option<(String, MethodType)>);
	fn visit_sourcefile(&mut self, source_file: String);
	fn visit_source_debug_extension(&mut self, extension: Bytes);
	fn visit_deprecated(&mut self);
}

pub trait MethodVisitor {
	type CodeVisitorType: CodeVisitor;
	fn visit_code(&mut self) -> Option<Self::CodeVisitorType>;
	fn visit_signature(&mut self, sig: String);
	fn visit_exceptions(&mut self, exceptions: Vec<ClassRef>);
}

pub trait CodeVisitor {
	type EndReturn;
	
	fn visit_opcode(&mut self, opcode: Opcode);
	fn visit_label(&mut self, label: Label);
	fn visit_maxs(&mut self, max_stack: u16, max_locals: u16);
	fn visit_exception_handler(&mut self, handler: ExceptionHandler);
	fn visit_frame(&mut self, frame: StackFrame);
	fn visit_end(&mut self) -> Self::EndReturn; // some CVs do computations here and might want to return something, usually a Result
}

pub trait FieldVisitor {
	fn visit_constant_value(&mut self, value: JFieldConst);
	fn visit_signature(&mut self, sig: String);
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

#[derive(Clone, Debug, PartialEq)]
pub enum JFieldConst {
	Int(i32),
	Float(f32),
	Long(i64),
	Double(f64),
	Str(String)
}

pub struct InnerClassInfo {
	class: ClassRef,
	name: Option<String>,
	outer_class: Option<ClassRef>,
	access: InnerClassAccess
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

pub trait GetOffset {
	fn get_offset(&self) -> u16;
}

pub trait Serialize {
	fn serialize(&self) -> Bytes;
}
