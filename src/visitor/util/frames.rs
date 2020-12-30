use crate::{
	visitor::*,
	visitor::util::delegate::*,
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

impl<T: ClassVisitor> DelegatingClassVisitor for FrameComputingClassVisitor<T> where <T::MethodVisitorType as MethodVisitor>::CodeVisitorType: GetOffset {
	type DelegateType = T;
	type MethodVisitorType = FrameComputingMethodVisitor<T::MethodVisitorType>;
	type FieldVisitorType = T::FieldVisitorType;
	
	fn get_delegate(&mut self) -> &mut Self::DelegateType {
		&mut self.delegate
	}
	
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

impl<T: MethodVisitor> DelegatingMethodVisitor for FrameComputingMethodVisitor<T> where T::CodeVisitorType: GetOffset {
	type DelegateType = T;
	type CodeVisitorType = FrameComputingCodeVisitor<T::CodeVisitorType>;
	
	fn get_delegate(&mut self) -> &mut Self::DelegateType {
		&mut self.delegate
	}
	
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

impl<T: CodeVisitor + GetOffset> DelegatingCodeVisitor for FrameComputingCodeVisitor<T> {
	type DelegateType = T;
	type EndReturn = Result<(), EffectApplyErr>;
	fn get_delegate(&mut self) -> &mut Self::DelegateType {
		&mut self.delegate
	}
	
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
			If(_, l) | IfNull(l) | IfNonNull(l) => {
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Jump(l.clone()));
			}
			IfICmp(_, l) | IfACmp(_, l) => {
				stack_effect.push(StackEffect::Pop(2));
				stack_effect.push(StackEffect::Jump(l.clone()));
			}
			Goto(l) => {
				stack_effect.push(StackEffect::Jump(l.clone()));
				stack_effect.push(StackEffect::End);
			}
			TableSwitch(l, _, _, ls) => {
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Jump(l.clone()));
				for la in ls {
					stack_effect.push(StackEffect::Jump(la.clone()));
				}
			}
			LookupSwitch(l, ls) => {
				stack_effect.push(StackEffect::Pop(1));
				stack_effect.push(StackEffect::Jump(l.clone()));
				for (_, la) in ls {
					stack_effect.push(StackEffect::Jump(la.clone()));
				}
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
		self.stack_effects.push(stack_effect);
		self.delegate.visit_opcode(opcode)
	}
	fn visit_maxs(&mut self, _max_stack: u16, _max_locals: u16) { }
	
	fn visit_end(&mut self) -> Self::EndReturn {
		let mut ls = self.jumps_to_process.clone();
		let mut block_map = HashMap::new();
		ls.sort();
		let l = Label::new();
		CodeVisitor::visit_label(self, l.clone());
		ls.push(l);
		let mut last_label = self.start_label.clone();
		let mut start_offset = 0;
		let mut first_block = None;
		for label in ls {
			let offset = label.info.get().unwrap().opcode_offset as usize;
			let mut block_opcodes: Vec<StackEffect> = self.stack_effects[start_offset..offset].iter().flatten().cloned().collect();
			block_opcodes.push(StackEffect::Jump(label.clone()));
			start_offset = offset;
			let block = Rc::new(StackBlock {
				start: last_label.clone(),
				effects: block_opcodes,
				processed: Cell::new(false)
			});
			if first_block.is_none() {
				first_block = Some(block.clone());
			}
			block_map.insert(last_label, block);
			last_label = label;
		}
		let mut statemap = FrameStorage {
			store: HashMap::new()
		};
		if let Some(b) = first_block {
			b.compute_states(&mut self.initial_state.clone(), &block_map, &mut statemap)?;
		}
		let mut iter = statemap.store.iter();
		let mut last_state = iter.next().unwrap().1; // first state -- we don't want to visit the frame here, it's automatically inferred by the JVM
		let mut max_stack = last_state.max_stack;
		let mut max_locals = last_state.locals.len() as u16;
		for (label, state) in iter {
			self.delegate.visit_frame(compute_frame(last_state, state, label.clone()));
			last_state = state;
			max_stack = max_stack.max(last_state.max_stack);
			max_locals = max_locals.max(last_state.locals.len() as u16);
		}
		self.delegate.visit_maxs(max_stack, max_locals);
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
	} else if locals_cmp_thing(first_state, second_state) && second_state.stack.is_empty() {
		StackFrame::Chop(pos, (first_state.locals.len() - second_state.locals.len()) as u8)
	} else if locals_cmp_thing(second_state, first_state) && second_state.stack.is_empty() {
		StackFrame::Append(pos, second_state.locals[first_state.locals.len()..].into())
	} else {
		StackFrame::Full(pos, second_state.locals.clone(), second_state.stack.clone())
	}
}

fn locals_cmp_thing(first: &JStackState, second: &JStackState) -> bool {
	if second.locals.len() < first.locals.len() && first.locals.len() - second.locals.len() <= 3 {
		for i in 0..second.locals.len() {
			if second.locals[i] != first.locals[i] {
				return false;
			}
		}
		true
	} else {
		false
	}
}

impl<T: CodeVisitor + GetOffset> GetOffset for FrameComputingCodeVisitor<T> {
	fn get_offset(&self) -> u16 {
		self.delegate.get_offset()
	}
}

struct FrameStorage {
	store: HashMap<Label, JStackState>
}

impl FrameStorage {
	fn put(&mut self, pos: Label, state: JStackState) {
		self.store.entry(pos).and_modify(|v| {
			if v.locals.len() > state.locals.len() {
				*v = state.clone();
			}
		}).or_insert(state);
	}
}

#[derive(Debug, Clone)]
struct StackBlock {
	start: Label,
	effects: Vec<StackEffect>,
	processed: Cell<bool>
}

impl StackBlock {
	fn compute_states(&self, initial_state: &mut JStackState, label_mapper: &HashMap<Label, Rc<StackBlock>>, state_map: &mut FrameStorage) -> Result<(), EffectApplyErr> {
		if self.processed.replace(true) {
			return Ok(())
		}
		state_map.put(self.start.clone(), initial_state.clone());
		for effect in &self.effects {
			if !effect.apply(initial_state, label_mapper, state_map)? {
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
	fn apply(&self, state: &mut JStackState, label_mapper: &HashMap<Label, Rc<StackBlock>>, state_map: &mut FrameStorage) -> Result<bool, EffectApplyErr> {
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
				if let Some(Uninitialized(i)) = state.peek(0) {
					let i = *i;
					state.initialize(i, Class(r.clone()).into())
				} else if let Some(UninitializedThis) = state.peek(0) {
					state.initialize_this(Class(r.clone()).into())
				} else{
					return Err(EffectApplyErr::NonUninit)
				};
				
			}
			Self::Jump(l) => {
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
	locals: Vec<JVerificationType>,
	max_stack: u16
}

impl JStackState {
	fn new() -> Self {
		JStackState {
			stack: Vec::new(),
			locals: Vec::new(),
			max_stack: 0
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
		self.max_stack = self.max_stack.max(self.stack.len() as u16);
	}
	
	fn pop(&mut self) -> JVerificationType {
		self.stack.pop().unwrap()
	}
	
	fn peek(&self, offset: usize) -> Option<&JVerificationType> {
		self.stack.get(self.stack.len() - 1 - offset)
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
	
	fn initialize(&mut self, id: u16, to: JVerificationType) {
		for entry in &mut self.stack {
			if let Uninitialized(off) = entry {
				if id == *off {
					*entry = to.clone()
				}
			}
		}
	}
	
	fn initialize_this(&mut self, to: JVerificationType) {
		for entry in &mut self.stack {
			if let UninitializedThis = entry {
				*entry = to.clone();
			}
		}
	}
	
}