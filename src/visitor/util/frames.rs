use crate::{
	visitor::*,
	opcodes::*,
	types::*,
	implm::util::MaybeRef
};
use std::rc::Rc;
use std::collections::HashMap;
use std::cell::Cell;

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
	jumps_to_process: Vec<Label>, // to
	stack_effects: Vec<Vec<StackEffect>>,
	initial_state: JStackState,
	start_label: Label
}

impl<T: CodeVisitor + GetOffset> FrameComputingCodeVisitor<T> {
	pub fn new(mut delegate: T, ty: &MethodType, selftype: Option<ClassRef>) -> Self {
		let l = Label::new();
		delegate.visit_label(l.clone());
		FrameComputingCodeVisitor {
			delegate,
			jumps_to_process: Vec::new(),
			stack_effects: Vec::new(),
			initial_state: JStackState::from_method_type(ty, selftype),
			start_label: l
		}
	}
}

impl<T: CodeVisitor + GetOffset> CodeVisitor for FrameComputingCodeVisitor<T> {
	type EndReturn = Result<(), EffectApplyErr>;
	fn visit_opcode(&mut self, opcode: Opcode) {
		let mut stack_effect = Vec::new();
		
		
		match &opcode {
			Const(c) => {
				stack_effect.push(StackEffect::Push(IST::Type(c.get_verification_type())))
			}
			Load(t, i) => {
				let verif_t = match t {
					JStackType::Int => IST::Type(Int.into()),
					JStackType::Double => IST::Type(Double.into()),
					JStackType::Float => IST::Type(Float.into()),
					JStackType::Long => IST::Type(Long.into()),
					JStackType::Reference => IST::LocalRef(*i)
				};
				stack_effect.push(StackEffect::Push(verif_t));
			}
			Store(t, i) => {
				let verif_t = match t {
					JStackType::Int => IST::Type(Int.into()),
					JStackType::Double => IST::Type(Double.into()),
					JStackType::Float => IST::Type(Float.into()),
					JStackType::Long => IST::Type(Long.into()),
					JStackType::Reference => IST::StackTop(0)
				};
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::LocalType(*i, verif_t))
			}
			ArrayLoad(t) => {
				let verif_t = match t {
					JBasicType::Byte | JBasicType::Boolean | JBasicType::Short | JBasicType::Char | JBasicType::Int => IST::Type(Int.into()),
					JBasicType::Double => IST::Type(Double.into()),
					JBasicType::Float => IST::Type(Float.into()),
					JBasicType::Long => IST::Type(Long.into()),
					JBasicType::Reference => IST::ArrayTop
				};
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Insert(2, verif_t));
				stack_effect.push(StackEffect::Pop(1));
			}
			ArrayStore(_) => {
				stack_effect.push(StackEffect::Pop(3));
			}
			Dup => {
				stack_effect.push(StackEffect::Push(IST::StackTop(0)));
			}
			Dup2 => {
				stack_effect.push(StackEffect::Push(IST::StackTop(1)));
				stack_effect.push(StackEffect::Push(IST::StackTop(0)));
			}
			DupX1 => {
				stack_effect.push(StackEffect::Insert(2, IST::StackTop(0)));
			}
			DupX2 => {
				stack_effect.push(StackEffect::Insert(3, IST::StackTop(0)));
			}
			Dup2X1 => {
				stack_effect.push(StackEffect::Insert(2, IST::StackTop(1)));
				stack_effect.push(StackEffect::Insert(2, IST::StackTop(0)));
			}
			Dup2X2 => {
				stack_effect.push(StackEffect::Insert(3, IST::StackTop(1)));
				stack_effect.push(StackEffect::Insert(3, IST::StackTop(0)));
			}
			Add(_) | Sub(_) | Mul(_) | Div(_) | Rem(_) | And(_) | Or(_) | Xor(_) => {
				stack_effect.push(StackEffect::Pop(1));
			}
			NumConvert(_, t) => {
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Push(IST::Type(JType::from(*t).get_verification_type())));
			}
			IConvert(t) => {
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Push(IST::Type(JType::from(*t).get_verification_type())));
			}
			LCmp | FCmpL | FCmpG | DCmpL | DCmpG => {
				stack_effect.push(StackEffect::Pop(2));
				stack_effect.push(StackEffect::Push(IST::Type(Int.into())));
			}
			If(_, _) | IfNull(_) | IfNonNull(_) => {
				stack_effect.push(StackEffect::Pop(1));
			}
			IfICmp(_, _) | IfACmp(_, _) => {
				stack_effect.push(StackEffect::Pop(2));
			}
			Return(_) | AThrow => {
				stack_effect.push(StackEffect::End);
			}
			GetField(f) => {
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Push(IST::Type(f.get_type().get_verification_type())));
			}
			PutField(_) => {
				stack_effect.push(StackEffect::Pop(2));
			}
			GetStatic(f) => {
				stack_effect.push(StackEffect::Push(IST::Type(f.get_type().get_verification_type())));
			}
			PutStatic(_) => {
				stack_effect.push(StackEffect::Pop(1));
			}
			InvokeVirtual(m, _) | InvokeSpecial(m, _) | InvokeInterface(m) => {
				if m.get_name() == "<init>" {
					stack_effect.push(StackEffect::Initialize(m.get_class().clone()));
				}
				stack_effect.push(StackEffect::Pop(m.get_type().get_args().len() + 1));
				if let Some(r) = m.get_type().get_return() {
					stack_effect.push(StackEffect::Push(IST::Type(r.get_verification_type())));
				}
			}
			InvokeStatic(m, _) => {
				stack_effect.push(StackEffect::Pop(m.get_type().get_args().len()));
				if let Some(r) = m.get_type().get_return() {
					stack_effect.push(StackEffect::Push(IST::Type(r.get_verification_type())));
				}
			}
			InvokeDynamic(c) => {
				stack_effect.push(StackEffect::Pop(c.ty.get_args().len()));
				if let Some(r) = c.ty.get_return() {
					stack_effect.push(StackEffect::Push(IST::Type(r.get_verification_type())));
				}
			}
			New(_) => {
				stack_effect.push(StackEffect::Push(IST::Type(Uninitialized(self.get_offset()))));
			}
			NewArray(t) => {
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Push(IST::Type(Array(t.clone()).into())));
			}
			MultiANewArray(t, d) => {
				stack_effect.push(StackEffect::Pop((*d).into()));
				stack_effect.push(StackEffect::Push(IST::Type(t.clone().into())));
			}
			ArrayLength => {
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Push(IST::Type(Int.into())));
			}
			Pop => {
				stack_effect.push(StackEffect::Pop(1));
			}
			Pop2 => {
				stack_effect.push(StackEffect::Pop(2));
			}
			Swap => {
				stack_effect.push(StackEffect::Insert(2, IST::StackTop(0)));
				stack_effect.push(StackEffect::Pop(1));
			}
			_ => ()
		};
		
		if let If(_, l) | IfICmp(_, l) | IfACmp(_, l) | Goto(l) | IfNull(l) | IfNonNull(l) = &opcode {
			self.jumps_to_process.push(l.clone());
			stack_effect.push(StackEffect::Jump(l.clone()));
			if let Goto(_) = &opcode {
				stack_effect.push(StackEffect::End);
			}
		}
		self.stack_effects.push(stack_effect);
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
	
	fn visit_end(&mut self) -> Self::EndReturn {
		let mut ls = self.jumps_to_process.clone();
		let mut block_map = HashMap::new();
		ls.sort();
		let l = Label::new();
		self.visit_label(l.clone());
		ls.push(l);
		let mut last_label = self.start_label.clone();
		let mut start_offset = 0;
		let mut first_block = None;
		println!("computing frames for labels: {:?}", ls);
		for label in ls {
			println!("{:?}", label);
			let offset = label.info.get().unwrap().opcode_offset as usize;
			let mut block_opcodes: Vec<StackEffect> = self.stack_effects[start_offset..offset].iter().flatten().cloned().collect();
			block_opcodes.push(StackEffect::Jump(label.clone()));
			start_offset = offset;
			let block = Rc::new(StackBlock {
				start: last_label.clone(),
				effects: block_opcodes,
				processed: Cell::new(false)
			});
			println!("{:?}", block);
			if first_block.is_none() {
				first_block = Some(block.clone());
			}
			block_map.insert(last_label, block);
			last_label = label;
		}
		let mut statemap = Vec::new();
		if first_block.is_some() {
			first_block.unwrap().compute_states(&mut self.initial_state.clone(), &block_map, &mut statemap).unwrap();
		}
		let mut iter = statemap.iter();
		let mut last_state = &iter.next().unwrap().1; // first state -- we don't want to visit the frame here, it's automatically inferred by the JVM
		for (label, state) in iter {
			self.delegate.visit_frame(compute_frame(last_state, state, label.clone()));
			last_state = state;
		}
		
		self.delegate.visit_end();
		Ok(())
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

#[derive(Debug, Clone)]
struct StackBlock {
	start: Label,
	effects: Vec<StackEffect>,
	processed: Cell<bool>
}

impl StackBlock {
	fn compute_states(&self, initial_state: &mut JStackState, label_mapper: &HashMap<Label, Rc<StackBlock>>, state_map: &mut Vec<(Label, JStackState)>) -> Result<(), EffectApplyErr> {
		if self.processed.replace(true) {
			return Ok(())
		}
		println!("State computed for block @ {:?}: {:?}", self.start, initial_state);
		state_map.push((self.start.clone(), initial_state.clone()));
		for effect in &self.effects {
			if !effect.apply(initial_state, label_mapper, state_map, self)? {
				break;
			}
		}
		Ok(())
	}
}

// internal stack type
#[derive(Clone, Debug, PartialEq, Eq)]
enum IST {
	LocalRef(u16),
	StackTop(usize),
	Type(JVerificationType),
	ArrayTop
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EffectApplyErr {
	NoLocal(u16),
	StackNotDeepEnough(usize),
	NonArray,
	NonUninit,
	JumpToNonexistentBlock(Label)
}

impl IST {
	fn fetch_type<'a>(&'a self, state: &'a JStackState) -> Result<MaybeRef<'a, JVerificationType>, EffectApplyErr> {
		match self {
			Self::Type(t) => Ok(MaybeRef::Ref(t)),
			Self::StackTop(i) => {
				if let Some(t) = state.peek(*i) {
					Ok(MaybeRef::Ref(t))
				} else {
					Err(EffectApplyErr::StackNotDeepEnough(*i))
				}
			}
			Self::LocalRef(i) => {
				if let Some(t) = state.locals.get(*i as usize) {
					Ok(MaybeRef::Ref(t))
				} else {
					Err(EffectApplyErr::NoLocal(*i))
				}
			}
			Self::ArrayTop => {
				if let Some(JVerificationType::Class(JObjectType::Array(t))) = state.peek(0) {
					Ok(MaybeRef::Owned(t.get_verification_type()))
				} else {
					Err(EffectApplyErr::NonArray)
				}
			}
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum StackEffect {
	Pop(usize),
	Push(IST),
	LocalType(u16, IST),
	Insert(usize, IST),
	Jump(Label),
	Initialize(ClassRef),
	End
}

impl StackEffect {
	fn apply(&self, state: &mut JStackState, label_mapper: &HashMap<Label, Rc<StackBlock>>, state_map: &mut Vec<(Label, JStackState)>, debug_bl: &StackBlock) -> Result<bool, EffectApplyErr> {
		//println!("{:?}", state);
		match self {
			Self::Pop(s) => {
				for _ in 0..*s {
					state.pop();
				}
			}
			Self::Push(t) => {
				state.push(t.fetch_type(state)?.clone());
			}
			Self::LocalType(i, t) => {
				state.set_local(*i, t.fetch_type(state)?.clone());
			}
			Self::Insert(i, t) => {
				if state.stack.len() >= *i {
					state.stack.insert(state.stack.len() - *i as usize, t.fetch_type(state)?.clone());
				} else {
					return Err(EffectApplyErr::StackNotDeepEnough(*i));
				}
			}
			Self::Initialize(r) => {
				let i = if let Some(Uninitialized(i)) = state.peek(0) {
					*i
				} else {
					return Err(EffectApplyErr::NonUninit)
				};
				state.initialize(i, JObjectType::Class(r.clone()))
			}
			Self::Jump(l) => {
				//println!("Jump to {:?} from block {:?}", l, debug_bl);
				if let Some(block) = label_mapper.get(l) {
					block.compute_states(&mut state.clone(), label_mapper, state_map)?;
				} else {
					return Err(EffectApplyErr::JumpToNonexistentBlock(l.clone()));
				}
			}
			Self::End => return Ok(false)
		};
		Ok(true)
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
	
	fn peek(&self, offset: usize) -> Option<&JVerificationType> {
		self.stack.get(self.stack.len() - 1 - offset)
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
		if index as usize == self.locals.len() {
			self.locals.push(ty);
		} else {
			self.locals[index as usize] = ty;
		}
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