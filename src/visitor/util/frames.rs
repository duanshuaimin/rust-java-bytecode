use crate::{
	visitor::*,
	opcodes::*,
	types::*
};

pub struct FrameComputingClassVisitorFactory<T: ClassVisitorFactory> where <<T::ClassVisitorType as ClassVisitor>::MethodVisitorType as MethodVisitor>::CodeVisitorType: GetOffset {
	delegate: T
}

impl<T: ClassVisitorFactory> ClassVisitorFactory for FrameComputingClassVisitorFactory<T> where <<T::ClassVisitorType as ClassVisitor>::MethodVisitorType as MethodVisitor>::CodeVisitorType: GetOffset {
	type ClassVisitorType = FrameComputingClassVisitor<T::ClassVisitorType>;
	fn create(&self, access: ClassAccess, name: String, superclass: ClassRef, interfaces: Vec<ClassRef>, major: u16, minor: u16) -> Self::ClassVisitorType {
		FrameComputingClassVisitor::new(self.delegate.create(access, name, superclass, interfaces, major, minor))
	}
}

impl<T: ClassVisitorFactory> FrameComputingClassVisitorFactory<T> where <<T::ClassVisitorType as ClassVisitor>::MethodVisitorType as MethodVisitor>::CodeVisitorType: GetOffset {
	pub fn new(delegate: T) -> Self {
		FrameComputingClassVisitorFactory{delegate}
	}
}

impl<T: ClassVisitor> FrameComputingClassVisitor<T> where <T::MethodVisitorType as MethodVisitor>::CodeVisitorType: GetOffset {
	pub fn new(delegate: T) -> Self {
		FrameComputingClassVisitor {
			delegate
		}
	}
}

pub struct FrameComputingClassVisitor<T: ClassVisitor> where <T::MethodVisitorType as MethodVisitor>::CodeVisitorType: GetOffset {
	delegate: T,
}

impl<T: ClassVisitor> ClassVisitor for FrameComputingClassVisitor<T> where <T::MethodVisitorType as MethodVisitor>::CodeVisitorType: GetOffset {
	type MethodVisitorType = FrameComputingMethodVisitor<T::MethodVisitorType>;
	type FieldVisitorType = T::FieldVisitorType;
	
	fn visit_method(&mut self, access: MethodAccess, name: String, ty: MethodType) -> Option<Self::MethodVisitorType> {
		let s = access.a_static;
		Some(FrameComputingMethodVisitor::new(self.delegate.visit_method(access, name.clone(), ty.clone())?, ty, name, s))
	}
	
	fn visit_field(&mut self, access: FieldAccess, name: String, ty: JType) -> Option<Self::FieldVisitorType> {
		self.delegate.visit_field(access, name, ty)
	}
}

impl<T: ClassVisitor + Serialize> Serialize for FrameComputingClassVisitor<T> where <T::MethodVisitorType as MethodVisitor>::CodeVisitorType: GetOffset {
	fn serialize(&self) -> Bytes {
		self.delegate.serialize()
	}
}

pub struct FrameComputingMethodVisitor<T: MethodVisitor> where T::CodeVisitorType: GetOffset {
	delegate: T,
	ty: MethodType,
	owner: ClassRef,
	is_static: bool
}

impl<T: MethodVisitor> MethodVisitor for FrameComputingMethodVisitor<T> where T::CodeVisitorType: GetOffset {
	type CodeVisitorType = FrameComputingCodeVisitor<T::CodeVisitorType>;
	fn visit_code(&mut self) -> Option<Self::CodeVisitorType> {
		Some(FrameComputingCodeVisitor::new(self.delegate.visit_code()?, &self.ty, if self.is_static {None} else {Some(self.owner.clone())}))
	}
}

impl<T: MethodVisitor> FrameComputingMethodVisitor<T> where T::CodeVisitorType: GetOffset {
	pub fn new(delegate: T, ty: MethodType, owner: ClassRef, is_static: bool) -> Self {
		FrameComputingMethodVisitor {
			delegate, ty, owner, is_static
		}
	}
}

pub struct FrameComputingCodeVisitor<T: CodeVisitor + GetOffset> {
	delegate: T,
	jumps_to_process: Vec<(u16, Label)>,
	stack_states: Vec<JStackState>,
	current_stack_state: JStackState,
	opcodes_visited: u16
}

impl<T: CodeVisitor + GetOffset> FrameComputingCodeVisitor<T> {
	pub fn new(delegate: T, ty: &MethodType, selftype: Option<ClassRef>) -> Self {
		FrameComputingCodeVisitor {
			delegate,
			jumps_to_process: Vec::new(),
			stack_states: Vec::new(),
			current_stack_state: JStackState::from_method_type(ty, selftype),
			opcodes_visited: 0
		}
	}
}

impl<T: CodeVisitor + GetOffset> CodeVisitor for FrameComputingCodeVisitor<T> {
	fn visit_opcode(&mut self, opcode: Opcode) {
		self.stack_states.push(self.current_stack_state.clone());
		if let If(_, l) | IfICmp(_, l) | IfACmp(_, l) | Goto(l) | IfNull(l) | IfNonNull(l) = &opcode {
			self.jumps_to_process.push((self.opcodes_visited, l.clone()))
		}
		
		match &opcode {
			Const(c) => {
				self.current_stack_state.push(c.get_verification_type());
			}
			Load(t, i) => {
				let verif_t = match t {
					JStackType::Int => Int.into(),
					JStackType::Double => Double.into(),
					JStackType::Float => Float.into(),
					JStackType::Long => Long.into(),
					JStackType::Reference => self.current_stack_state.locals[*i as usize].clone()
				};
				self.current_stack_state.push(verif_t);
			}
			Store(t, i) => {
				let verif_t = match t {
					JStackType::Int => Int.into(),
					JStackType::Double => Double.into(),
					JStackType::Float => Float.into(),
					JStackType::Long => Long.into(),
					JStackType::Reference => self.current_stack_state.peek(0).clone()
				};
				self.current_stack_state.pop();
				self.current_stack_state.set_local(*i, verif_t);
			}
			ArrayLoad(t) => {
				let verif_t = match t {
					JBasicType::Byte | JBasicType::Boolean | JBasicType::Short | JBasicType::Char | JBasicType::Int => Int.into(),
					JBasicType::Float => Float.into(),
					JBasicType::Long => Long.into(),
					JBasicType::Double => Double.into(),
					JBasicType::Reference => self.current_stack_state.peek(0).clone()
				};
				self.current_stack_state.pop();
				self.current_stack_state.pop();
				self.current_stack_state.push(verif_t);
			}
			ArrayStore(_) => {
				self.current_stack_state.pop();
				self.current_stack_state.pop();
				self.current_stack_state.pop();
			}
			Dup => {
				self.current_stack_state.push(self.current_stack_state.peek(0).clone());
			}
			Dup2 => {
				let (t1, t2) = (self.current_stack_state.peek(1).clone(), self.current_stack_state.peek(0).clone());
				self.current_stack_state.push(t1);
				self.current_stack_state.push(t2);
			}
			DupX1 => {
				let state1 = self.current_stack_state.pop();
				let state2 = self.current_stack_state.pop();
				self.current_stack_state.push(state1.clone());
				self.current_stack_state.push(state2);
				self.current_stack_state.push(state1);
			}
			DupX2 => {
				let state1 = self.current_stack_state.pop();
				let state2 = self.current_stack_state.pop();
				let state3 = self.current_stack_state.pop();
				self.current_stack_state.push(state1.clone());
				self.current_stack_state.push(state3);
				self.current_stack_state.push(state2);
				self.current_stack_state.push(state1);
			}
			Dup2X1 => {
				let (t2, t1) = (self.current_stack_state.pop(), self.current_stack_state.pop());
				let t3 = self.current_stack_state.pop();
				self.current_stack_state.push(t1.clone());
				self.current_stack_state.push(t2.clone());
				self.current_stack_state.push(t3);
				self.current_stack_state.push(t1);
				self.current_stack_state.push(t2);
			}
			Dup2X2 => {
				let (t2, t1) = (self.current_stack_state.pop(), self.current_stack_state.pop());
				let t3 = self.current_stack_state.pop();
				let t4 = self.current_stack_state.pop();
				self.current_stack_state.push(t1.clone());
				self.current_stack_state.push(t2.clone());
				self.current_stack_state.push(t4);
				self.current_stack_state.push(t3);
				self.current_stack_state.push(t1);
				self.current_stack_state.push(t2);
			}
			Add(_) | Sub(_) | Mul(_) | Div(_) | Rem(_) | And(_) | Or(_) | Xor(_) => {
				self.current_stack_state.pop();
			}
			NumConvert(_, t) => {
				self.current_stack_state.pop();
				self.current_stack_state.push(JType::from(*t).get_verification_type());
			}
			IConvert(t) => {
				self.current_stack_state.pop();
				self.current_stack_state.push(JType::from(*t).get_verification_type());
			}
			LCmp | FCmpL | FCmpG | DCmpL | DCmpG => {
				self.current_stack_state.pop();
				self.current_stack_state.pop();
				self.current_stack_state.push(Int.into());
			}
			If(_, _) | IfNull(_) | IfNonNull(_) => {
				self.current_stack_state.pop();
			}
			IfICmp(_, _) | IfACmp(_, _) => {
				self.current_stack_state.pop();
				self.current_stack_state.pop();
			}
			Return(_) | Goto(_) | AThrow => {
				self.current_stack_state.clear();
			}
			GetField(f) => {
				self.current_stack_state.pop();
				self.current_stack_state.push(f.get_type().get_verification_type());
			}
			PutField(_) => {
				self.current_stack_state.pop();
				self.current_stack_state.pop();
			}
			GetStatic(f) => {
				self.current_stack_state.push(f.get_type().get_verification_type());
			}
			PutStatic(_) => {
				self.current_stack_state.pop();
			}
			InvokeVirtual(m, _) | InvokeSpecial(m, _) | InvokeInterface(m) => {
				let top = self.current_stack_state.pop();
				if let Uninitialized(u) = top {
					self.current_stack_state.initialize(u, Class(m.get_class().clone()))
				}
				for _ in 0..m.get_type().get_args().len() {
					self.current_stack_state.pop();
				}
				if let Some(r) = m.get_type().get_return() {
					self.current_stack_state.push(r.get_verification_type());
				}
			}
			InvokeStatic(m, _) => {
				for _ in 0..m.get_type().get_args().len() {
					self.current_stack_state.pop();
				}
				if let Some(r) = m.get_type().get_return() {
					self.current_stack_state.push(r.get_verification_type());
				}
			}
			InvokeDynamic(c) => {
				for _ in 0..c.ty.get_args().len() {
					self.current_stack_state.pop();
				}
				if let Some(r) = c.ty.get_return() {
					self.current_stack_state.push(r.get_verification_type());
				}
			}
			New(_) => {
				self.current_stack_state.push(Uninitialized(self.get_offset()));
			}
			NewArray(t) => {
				self.current_stack_state.pop();
				self.current_stack_state.push(Array(t.clone()).into());
			}
			MultiANewArray(t, d) => {
				for _ in 0..*d {
					self.current_stack_state.pop();
				}
				self.current_stack_state.push(t.clone().into());
			}
			ArrayLength => {
				self.current_stack_state.pop();
				self.current_stack_state.push(Int.into());
			}
			_ => ()
		};
		self.opcodes_visited += 1;
		self.delegate.visit_opcode(opcode)
	}
	
	fn visit_label(&mut self, label: Label) {
		self.delegate.visit_label(label)
	}
	
	fn visit_maxs(&mut self, max_stack: u16, max_locals: u16) {
		self.delegate.visit_maxs(max_stack, max_locals)
	}
	
	fn visit_exception_handler(&mut self, handler: ExceptionHandler) {
		self.delegate.visit_exception_handler(handler)
	}
	
	fn visit_end(&mut self) {
		let mut last_state = &self.stack_states[0];
		for (pos, label) in &self.jumps_to_process {
			if let Some(info) = label.info.get() {
				let mut state = &self.stack_states[info.opcode_offset as usize];
				if state.stack.is_empty() && state.locals.is_empty() {
					state = &self.stack_states[*pos as usize];
				}
				println!("{:?} at {}", state, info.opcode_offset);
				self.delegate.visit_frame(compute_frame(last_state, state, label.clone()));
				last_state = state;
			}
		}
		
		self.delegate.visit_end()
	}
	
	fn visit_frame(&mut self, _frame: StackFrame) { }
}

fn compute_frame(first_state: &JStackState, second_state: &JStackState, pos: Label) -> StackFrame {
	if first_state.locals == second_state.locals && second_state.stack.is_empty() {
		StackFrame::Same(pos)
	} else if first_state.locals == second_state.locals && second_state.stack.len() == 1 {
		StackFrame::SameLocals1Stack(pos, second_state.stack[1].clone())
	} else {
		StackFrame::Full(pos, second_state.locals.clone(), second_state.stack.clone())
	}
}

impl<T: CodeVisitor + GetOffset> GetOffset for FrameComputingCodeVisitor<T> {
	fn get_offset(&self) -> u16 {
		self.delegate.get_offset()
	}
}

#[derive(Clone, Debug, PartialEq)]
struct JStackState {
	stack: Vec<JVerificationType>,
	locals: Vec<JVerificationType>
}

impl JStackState {
	fn new() -> Self {
		JStackState {
			stack: Vec::new(),
			locals: Vec::new()
		}
	}
	
	fn from_method_type(ty: &MethodType, selftype: Option<ClassRef>) -> Self {
		let mut state = Self::new();
		if let Some(t) = selftype {
			state.locals.push(Class(t).into());
		}
		for arg_type in ty.get_args() {
			state.locals.push(arg_type.get_verification_type());
			if arg_type.get_verification_type().is_wide() {
				state.locals.push(Top)
			}
		}
		state
	}
	
	fn push(&mut self, ty: JVerificationType) {
		self.stack.push(ty);
	}
	
	fn pop(&mut self) -> JVerificationType {
		self.stack.pop().unwrap()
	}
	
	fn peek(&self, offset: usize) -> &JVerificationType {
		&self.stack[self.stack.len() - 1 - offset]
	}
	
	fn clear(&mut self) {
		self.stack.clear();
		self.locals.clear();
	}
	
	fn set_local(&mut self, index: u16, ty: JVerificationType) {
		while index > self.locals.len() as u16 {
			self.locals.push(Top);
		}
		let wide = ty.is_wide();
		self.locals.push(ty);
		if wide && index != self.locals.len() as u16 - 1 {
			self.locals[index as usize + 1] = Top;
		}
	}
	
	fn initialize(&mut self, id: u16, to: JObjectType) {
		let v_to: JVerificationType = to.into();
		for entry in &mut self.stack {
			if let Uninitialized(off) = entry {
				if id == *off {
					*entry = v_to.clone()
				}
			}
		}
	}
}