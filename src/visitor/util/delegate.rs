use crate::visitor::*;

pub trait DelegatingClassVisitor {
	type DelegateType: ClassVisitor;
	type MethodVisitorType: MethodVisitor;
	type FieldVisitorType: FieldVisitor;
	
	fn get_delegate(&mut self) -> &mut Self::DelegateType;
	fn visit_method(&mut self, access: MethodAccess, name: String, desc: MethodType) -> Option<Self::MethodVisitorType>;
	fn visit_field(&mut self, access: FieldAccess, name: String, fieldtype: JType) -> Option<Self::FieldVisitorType>;
	
	fn visit_signature(&mut self, sig: String) {
		self.get_delegate().visit_signature(sig)
	}
	fn visit_inner_classes(&mut self, classes: Vec<InnerClassInfo>) {
		self.get_delegate().visit_inner_classes(classes)
	}
	fn visit_enclosing_method(&mut self, outer_class: ClassRef, outer_method: Option<(String, MethodType)>) {
		self.get_delegate().visit_enclosing_method(outer_class, outer_method)
	}
	fn visit_sourcefile(&mut self, source_file: String) {
		self.get_delegate().visit_sourcefile(source_file)
	}
	fn visit_source_debug_extension(&mut self, extension: Bytes) {
		self.get_delegate().visit_source_debug_extension(extension)
	}
	fn visit_deprecated(&mut self) {
		self.get_delegate().visit_deprecated()
	}
}

impl<T: DelegatingClassVisitor> ClassVisitor for T {
	type MethodVisitorType = T::MethodVisitorType;
	type FieldVisitorType = T::FieldVisitorType;
	
	fn visit_method(&mut self, access: MethodAccess, name: String, desc: MethodType) -> Option<Self::MethodVisitorType> {
		T::visit_method(self, access, name, desc)
	}
	fn visit_field(&mut self, access: FieldAccess, name: String, fieldtype: JType) -> Option<Self::FieldVisitorType> {
		T::visit_field(self, access, name, fieldtype)
	}
	
	fn visit_signature(&mut self, sig: String) {
		T::visit_signature(self, sig)
	}
	fn visit_inner_classes(&mut self, classes: Vec<InnerClassInfo>) {
		T::visit_inner_classes(self, classes)
	}
	fn visit_enclosing_method(&mut self, outer_class: ClassRef, outer_method: Option<(String, MethodType)>) {
		T::visit_enclosing_method(self, outer_class, outer_method)
	}
	fn visit_sourcefile(&mut self, source_file: String) {
		T::visit_sourcefile(self, source_file)
	}
	fn visit_source_debug_extension(&mut self, extension: Bytes) {
		T::visit_source_debug_extension(self, extension)
	}
	fn visit_deprecated(&mut self) {
		T::visit_deprecated(self)
	}
}

pub trait DelegatingMethodVisitor {
	type DelegateType: MethodVisitor;
	type CodeVisitorType: CodeVisitor;
	
	fn get_delegate(&mut self) -> &mut Self::DelegateType;
	fn visit_code(&mut self) -> Option<Self::CodeVisitorType>;
	
	fn visit_signature(&mut self, sig: String) {
		self.get_delegate().visit_signature(sig)
	}
	fn visit_exceptions(&mut self, exceptions: Vec<ClassRef>) {
		self.get_delegate().visit_exceptions(exceptions)
	}
}

impl<T: DelegatingMethodVisitor> MethodVisitor for T {
	type CodeVisitorType = T::CodeVisitorType;
	fn visit_code(&mut self) -> Option<Self::CodeVisitorType> {
		T::visit_code(self)
	}
	fn visit_signature(&mut self, sig: String) {
		T::visit_signature(self, sig)
	}
	fn visit_exceptions(&mut self, exceptions: Vec<ClassRef>) {
		T::visit_exceptions(self, exceptions)
	}
}

pub trait DelegatingCodeVisitor {
	type DelegateType: CodeVisitor;
	type EndReturn;
	fn get_delegate(&mut self) -> &mut Self::DelegateType;
	fn visit_end(&mut self) -> Self::EndReturn;
	
	fn visit_opcode(&mut self, opcode: Opcode) {
		self.get_delegate().visit_opcode(opcode)
	}
	fn visit_label(&mut self, label: Label) {
		self.get_delegate().visit_label(label)
	}
	fn visit_maxs(&mut self, max_stack: u16, max_locals: u16) {
		self.get_delegate().visit_maxs(max_stack, max_locals)
	}
	fn visit_exception_handler(&mut self, handler: ExceptionHandler) {
		self.get_delegate().visit_exception_handler(handler)
	}
	fn visit_frame(&mut self, frame: StackFrame) {
		self.get_delegate().visit_frame(frame)
	}
}

impl<T: DelegatingCodeVisitor> CodeVisitor for T {
	type EndReturn = T::EndReturn;
	fn visit_end(&mut self) -> Self::EndReturn {
		T::visit_end(self)
	}
	
	fn visit_opcode(&mut self, opcode: Opcode) {
		T::visit_opcode(self, opcode)
	}
	fn visit_label(&mut self, label: Label) {
		T::visit_label(self, label)
	}
	fn visit_maxs(&mut self, max_stack: u16, max_locals: u16) {
		T::visit_maxs(self, max_stack, max_locals)
	}
	fn visit_exception_handler(&mut self, handler: ExceptionHandler) {
		T::visit_exception_handler(self, handler)
	}
	fn visit_frame(&mut self, frame: StackFrame) {
		T::visit_frame(self, frame)
	}
}

pub trait DelegatingFieldVisitor {
	type DelegateType: FieldVisitor;
	fn get_delegate(&mut self) -> &mut Self::DelegateType;
	
	fn visit_constant_value(&mut self, value: JFieldConst) {
		self.get_delegate().visit_constant_value(value)
	}
	fn visit_signature(&mut self, sig: String) {
		self.get_delegate().visit_signature(sig)
	}
}

impl<T: DelegatingFieldVisitor> FieldVisitor for T {
	fn visit_constant_value(&mut self, value: JFieldConst) {
		T::visit_constant_value(self, value)
	}
	fn visit_signature(&mut self, sig: String) {
		T::visit_signature(self, sig)
	}
}
