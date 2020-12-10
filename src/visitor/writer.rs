use crate::{
	opcodes::{
		Opcode::*,
		Comparison::*,
		Label,
		RefComparison,
		ShiftType::*,
		JConst,
		LabelInfo
	},
	implm::{
		consts::{
			opcodes::*,
			types::*,
			frames::*
		},
		ConstantPool,
		AttributePool,
		Attribute,
		BootstrapMethodPool,
		AttrEnum
	},
	types::*,
	visitor::*
};
use std::{
	cell::RefCell,
	rc::Rc,
	convert::TryInto,
};
use ProcessedOpcode::*;
use ProcessedOpcode::Nop;
use bytes::{BytesMut, BufMut, Bytes};

pub struct ClassWriterFactory;

impl ClassVisitorFactory for ClassWriterFactory {
	type ClassVisitorType = ClassWriter;
	fn create(&self, access: ClassAccess, name: String, superclass: ClassRef, interfaces: Vec<ClassRef>, major: u16, minor: u16) -> Self::ClassVisitorType {
		ClassWriter::visit(access, name, superclass, interfaces, major, minor)
	}
}

impl ClassWriterFactory {
	pub fn new() -> Self {
		ClassWriterFactory{}
	}
}

pub struct ClassWriter {
	access: ClassAccess,
	name: u16,
	superclass: u16,
	interfaces: Vec<u16>,
	constant_pool: Rc<RefCell<ConstantPool>>,
	methods: Vec<Rc<RefCell<MethodWriter>>>,
	fields: Vec<Rc<RefCell<FieldWriter>>>,
	attrs: AttributePool,
	major: u16,
	minor: u16
}

pub struct MethodWriter {
	access: MethodAccess,
	name: u16,
	desc: u16,
	constant_pool: Rc<RefCell<ConstantPool>>,
	attrs: AttributePool,
}

pub struct CodeWriter {
	max_locals: u16,
	max_stack: u16,
	code_offset: u16,
	opcodes: Vec<ProcessedOpcode>,
	exceptions: Vec<ExceptionHandler>,
	constant_pool: Rc<RefCell<ConstantPool>>,
	attrs: AttributePool,
	main_buf: Option<Bytes>,
	frames: Option<Rc<RefCell<Vec<StackFrame>>>>
}

impl CodeVisitor for CodeWriter {
	
	fn visit_maxs(&mut self, max_stack: u16, max_locals: u16) {
		self.max_stack = max_stack;
		self.max_locals = max_locals;
	}
	
	fn visit_opcode(&mut self, opcode: Opcode) {
		let processed = ProcessedOpcode::process(opcode, &mut *self.constant_pool.borrow_mut());
		self.code_offset += processed.get_code_length(self.code_offset);
		self.opcodes.push(processed);
	}
	
	fn visit_label(&mut self, label: Label) {
		label.info.replace(Some(LabelInfo {
			offset: self.code_offset,
			opcode_offset: self.opcodes.len() as u16
		}));
	}
	
	fn visit_exception_handler(&mut self, handler: ExceptionHandler) {
		self.exceptions.push(handler);
	}
	
	fn visit_end(&mut self) {
		let mut opcode_buf = BytesMut::new();
		let mut code_offset = 0;
		for opcode in &self.opcodes {
			opcode.write(&mut opcode_buf, code_offset).unwrap();
			code_offset += opcode.get_code_length(code_offset);
		}
		
		let mut buf = BytesMut::new();
		buf.put_u16(self.max_stack);
		buf.put_u16(self.max_locals);
		buf.put_u32(opcode_buf.len() as u32);
		buf.put(opcode_buf);
		buf.put_u16(self.exceptions.len() as u16);
		for exception in &self.exceptions {
			buf.put_u16(get_label_exc(&exception.start, &exception.exception, ExceptionLabelType::Start).unwrap());
			buf.put_u16(get_label_exc(&exception.end, &exception.exception, ExceptionLabelType::End).unwrap());
			buf.put_u16(get_label_exc(&exception.handler, &exception.exception, ExceptionLabelType::Handler).unwrap());
			if let Some(r) = &exception.exception {
				buf.put_u16(self.constant_pool.borrow_mut().class_ref(r.into()));
			} else {
				buf.put_u16(0);
			}
		}
		self.main_buf = Some(buf.freeze());
	}
	
	fn visit_frame(&mut self, mut frame: StackFrame) {
		if self.frames.is_none() {
			self.frames = Some(Rc::new(RefCell::new(Vec::new())));
			self.attrs.put("StackMapTable".into(), AttrEnum::Frames(self.frames.clone().unwrap()), &mut *self.constant_pool.borrow_mut());
		}
		if let StackFrame::Full(_, l, s) = &mut frame {
			for local in l {
				if let JVerificationType::Class(c) = local {
					*local = JVerificationType::ClassInternal(self.constant_pool.borrow_mut().array_type_ref(c.clone().into()));
				}
			}
			for stack in s {
				if let JVerificationType::Class(c) = stack {
					*stack = JVerificationType::ClassInternal(self.constant_pool.borrow_mut().array_type_ref(c.clone().into()));
				}
			}
		}
		self.frames.as_ref().unwrap().borrow_mut().push(frame);
	}
}

impl Attribute for Vec<StackFrame> {
	fn serialize(&self, buf: &mut BytesMut) {
		let mut sorted = self.clone();
		sorted.sort_unstable();
		let mut last_offset = -1;
		buf.put_u16(sorted.len() as u16);
		for frame in sorted {
			let offset = frame.get_label().info.get().unwrap().offset as i32 - last_offset - 1;
			last_offset = offset;
			match frame {
				StackFrame::Full(_, l, s) => {
					buf.put_u8(F_FULL);
					buf.put_u16(offset as u16);
					buf.put_u16(l.len() as u16);
					for local in l {
						local.serialize(buf);
					}
					buf.put_u16(s.len() as u16);
					for stack in s {
						stack.serialize(buf);
						if stack.is_wide() {
							Top.serialize(buf);
						}
					}
				}
				StackFrame::Same(_) => {
					if offset <= 63 {
						buf.put_u8(F_SAME + offset as u8);
					} else {
						buf.put_u8(F_SAME_EXT);
						buf.put_u16(offset as u16);
					}
				}
				StackFrame::SameLocals1Stack(_, t) => {
					if offset <= 63 {
						buf.put_u8(F_SAME_LOCALS_1_STACK + offset as u8);
					} else {
						buf.put_u8(F_SAME_LOCALS_1_STACK_EXT);
						buf.put_u16(offset as u16);
					}
				}
				StackFrame::Chop(_, n) => {
					buf.put_u8(251 - n);
					buf.put_u16(offset as u16);
				}
				StackFrame::Append(_, l) => {
					buf.put_u8((F_APPEND - 1) + l.len() as u8);
					buf.put_u16(offset as u16);
					for local in l {
						local.serialize(buf);
					}
				}
			}
		}
	}
}

impl GetOffset for CodeWriter {
	fn get_offset(&self) -> u16 {
		self.code_offset
	}
}

impl Attribute for CodeWriter {
	fn serialize(&self, buf: &mut BytesMut) {
		if let Some(b) = self.main_buf.clone() {
			buf.put(b);
			buf.put(self.attrs.serialize());
		} else {
			panic!("visit_end not called before serialize?")
		}
	}
}

fn get_label_exc(l: &Label, err_c: &Option<ClassRef>, ty: ExceptionLabelType) -> Result<u16, InvalidLabelError> {
	if let Some(i) = l.info.get() {
		Ok(i.offset)
	} else {
		Err(InvalidLabelError::UnsetException{catching: err_c.clone(), ty})
	}
}

impl MethodVisitor for MethodWriter {
	type CodeVisitorType = Rc<RefCell<CodeWriter>>;
	fn visit_code(&mut self) -> Option<Self::CodeVisitorType> {
		let cw = Rc::new(RefCell::new(CodeWriter {
			max_locals: 0,
			max_stack: 0,
			code_offset: 0,
			opcodes: Vec::new(),
			constant_pool: self.constant_pool.clone(),
			main_buf: None,
			attrs: AttributePool::new(),
			exceptions: Vec::new(),
			frames: None
		}));
		if let AttrEnum::Code(cw) = self.attrs.put("Code".into(), AttrEnum::Code(cw), &mut *self.constant_pool.borrow_mut()) {
			Some(cw.clone())
		} else {
			panic!("Code attribute not a code writer!?")
		}
	}
}

impl CodeVisitor for Rc<RefCell<CodeWriter>> {
	fn visit_opcode(&mut self, opcode: Opcode) {
		self.borrow_mut().visit_opcode(opcode);
	}
	fn visit_label(&mut self, label: Label) {
		self.borrow_mut().visit_label(label);
	}
	fn visit_maxs(&mut self, max_stack: u16, max_locals: u16) {
		self.borrow_mut().visit_maxs(max_stack, max_locals);
	}
	fn visit_exception_handler(&mut self, handler: ExceptionHandler) {
		self.borrow_mut().visit_exception_handler(handler);
	}
	fn visit_end(&mut self) {
		self.borrow_mut().visit_end();
	}
	fn visit_frame(&mut self, frame: StackFrame) {
		self.borrow_mut().visit_frame(frame);
	}
}

impl GetOffset for Rc<RefCell<CodeWriter>> {
	fn get_offset(&self) -> u16 {
		self.borrow().get_offset()
	}
}

impl MethodWriter {
	fn serialize(&self, buf: &mut impl BufMut) {
		buf.put_u16(self.access.as_bitflag());
		buf.put_u16(self.name);
		buf.put_u16(self.desc);
		buf.put(self.attrs.serialize());
		println!("mattrs {:?}", self.attrs.serialize());
	}
}

impl ClassVisitor for ClassWriter {
	type MethodVisitorType = Rc<RefCell<MethodWriter>>;
	type FieldVisitorType = Rc<RefCell<FieldWriter>>;
	fn visit_method(&mut self, access: MethodAccess, name: String, desc: MethodType) -> Option<Self::MethodVisitorType> {
		let name = self.constant_pool.borrow_mut().utf8(name);
		let desc = self.constant_pool.borrow_mut().utf8(desc.descriptor());
		let mw = Rc::new(RefCell::new(MethodWriter {
			access, name, desc,
			constant_pool: self.constant_pool.clone(),
			attrs: AttributePool::new()
		}));
		
		self.methods.push(mw.clone());
		Some(mw)
	}
	
	fn visit_field(&mut self, access: FieldAccess, name: String, fieldtype: JType) -> Option<Self::FieldVisitorType> {
		let name = self.constant_pool.borrow_mut().utf8(name);
		let ty = self.constant_pool.borrow_mut().utf8(fieldtype.descriptor());
		let fw = Rc::new(RefCell::new(FieldWriter {
			name, ty, access,
			attrs: AttributePool::new(),
			constant_pool: self.constant_pool.clone()
		}));
		self.fields.push(fw.clone());
		Some(fw)
	}
}

impl MethodVisitor for Rc<RefCell<MethodWriter>> {
	type CodeVisitorType = Rc<RefCell<CodeWriter>>;
	fn visit_code(&mut self) -> Option<Self::CodeVisitorType> {
		self.borrow_mut().visit_code()
	}
}
impl FieldVisitor for Rc<RefCell<FieldWriter>> {
	
}

impl Serialize for ClassWriter {
	fn serialize(&self) -> Bytes {
		let mut buf = BytesMut::new();
		buf.put_u32(0xCAFEBABE); // magic
		buf.put_u16(self.minor);
		buf.put_u16(self.major);
		buf.put_u16(self.constant_pool.borrow().get_index());
		buf.put(self.constant_pool.borrow().serialize());
		buf.put_u16(self.access.as_bitflag());
		buf.put_u16(self.name);
		buf.put_u16(self.superclass);
		buf.put_u16(self.interfaces.len() as u16);
		for interface in &self.interfaces {
			buf.put_u16(*interface);
		}
		buf.put_u16(self.fields.len() as u16);
		for field in &self.fields {
			field.borrow().serialize(&mut buf);
		}
		
		buf.put_u16(self.methods.len() as u16);
		for method in &self.methods {
			method.borrow().serialize(&mut buf);
		}
		buf.put(self.attrs.serialize());
		
		buf.freeze()
	}
}

impl ClassWriter {
	pub fn visit(access: ClassAccess, name: ClassRef, superclass: ClassRef, interfaces: Vec<ClassRef>, major: u16, minor: u16) -> Self {
		let bootstrap_pool = Rc::new(RefCell::new(BootstrapMethodPool::new()));
		let mut cpool = ConstantPool::new(bootstrap_pool.clone());
		let name = cpool.class_ref(name);
		let superclass = cpool.class_ref(superclass);
		let mut ifaces = Vec::new();
		for interface in interfaces {
			ifaces.push(cpool.class_ref(interface));
		}
		let mut attrs = AttributePool::new();
		attrs.put("BootstrapMethods".into(), AttrEnum::Dyn(Box::new(bootstrap_pool)), &mut cpool);
		
		ClassWriter {
			access, name, superclass, interfaces: ifaces, major, minor,
			constant_pool: Rc::new(RefCell::new(cpool)),
			fields: Vec::new(),
			methods: Vec::new(),
			attrs
		}
	}
}

pub struct FieldWriter {
	access: FieldAccess,
	name: u16,
	ty: u16,
	attrs: AttributePool,
	constant_pool: Rc<RefCell<ConstantPool>>
}

impl FieldWriter {
	fn serialize(&self, buf: &mut impl BufMut) {
		buf.put_u16(self.access.as_bitflag());
		buf.put_u16(self.name);
		buf.put_u16(self.ty);
		buf.put(self.attrs.serialize());
	}
}

impl FieldVisitor for FieldWriter {
	
}

enum ProcessedOpcode {
	Nop, //special-cased so it can be easily stripped
	Basic(u8),
	LoadStore(u8, u16),
	Ldc(u8),
	LdcW(u16),
	Ldc2W(u16),
	Type(u8, u16),
	Invoke(u8, u16),
	InvokeDynamic(u16),
	Field(u8, u16),
	NewArray(u8),
	MultiNewArray(u16, u8),
	BIPush(i8),
	SIPush(i16),
	IInc(u16, i16),
	Jump(u8, Label),
	LookupSwitch(Label, Vec<(i32, Label)>),
	TableSwitch(Label, i32, i32, Vec<Label>)
}

#[derive(Debug)]
pub enum InvalidLabelError {
	UnsetJump {
		opcode: u8,
		code_offset: u16
	},
	TooFar {
		opcode: u8,
		code_offset: u16,
		label_pos: u16
	},
	UnsetException {
		catching: Option<ClassRef>,
		ty: ExceptionLabelType
	}
}

#[derive(Clone, Copy, Debug)]
pub enum ExceptionLabelType {
	Start, End, Handler
}

impl ProcessedOpcode {
	pub fn write(&self, buf: &mut impl BufMut, code_offset: u16) -> Result<(), InvalidLabelError> {
		match self {
			Basic(o) => buf.put_u8(*o),
			LoadStore(o, i) if *i <= U8_MAX_U16 => {buf.put_u8(*o); buf.put_u8(*i as u8)},
			LoadStore(o, i) => {buf.put_u8(WIDE); buf.put_u8(*o); buf.put_u16(*i)},
			Ldc(i) => {buf.put_u8(LDC); buf.put_u8(*i)}
			LdcW(i) => {buf.put_u8(LDC_W); buf.put_u16(*i)}
			Ldc2W(i) => {buf.put_u8(LDC2_W); buf.put_u16(*i)}
			Type(o, i) => {buf.put_u8(*o); buf.put_u16(*i)}
			Invoke(o, i) => {buf.put_u8(*o); buf.put_u16(*i)}
			Self::InvokeDynamic(i) => {buf.put_u8(INVOKEDYNAMIC); buf.put_u16(*i); buf.put_u16(0)} //java pls
			Field(o, i) => {buf.put_u8(*o); buf.put_u16(*i)}
			Self::NewArray(t) => {buf.put_u8(NEWARRAY); buf.put_u8(*t)}
			MultiNewArray(t, d) => {buf.put_u8(MULTIANEWARRAY); buf.put_u16(*t); buf.put_u8(*d)}
			BIPush(v) => {buf.put_u8(BIPUSH); buf.put_i8(*v)}
			SIPush(v) => {buf.put_u8(SIPUSH); buf.put_i16(*v)}
			Self::IInc(i, v) if *i <= U8_MAX_U16 && *v <= I8_MAX_I16 && *v >= I8_MAX_I16 => {buf.put_u8(IINC); buf.put_u8(*i as u8); buf.put_i8(*v as i8)}
			Self::IInc(i, v) => {buf.put_u8(WIDE); buf.put_u8(IINC); buf.put_u16(*i); buf.put_i16(*v)}
			Self::Jump(o, l) => {
				if let Some(i) = l.info.get() {
					let result: Result<i16, _> = (i.offset as i32 - code_offset as i32).try_into();
					if let Ok(offset) = result {
						buf.put_u8(*o);
						buf.put_i16(offset);
					} else {
						return Err(InvalidLabelError::TooFar{opcode: *o, code_offset, label_pos: i.offset})
					}
				} else {
					return Err(InvalidLabelError::UnsetJump{opcode: *o, code_offset});
				}
			},
			Self::LookupSwitch(d, m) => {
				if let Some(i) = d.info.get() {
					let default_offset = i.offset as i32 - code_offset as i32;
					buf.put_u8(LOOKUPSWITCH);
					let padding = 4 - ((code_offset + 1) % 4); // the jvm is a string of odd design descisions
					for _ in 0..padding {
						buf.put_u8(0);
					}
					buf.put_i32(default_offset);
					buf.put_i32(m.len() as i32);
					for entry in m {
						buf.put_i32(entry.0);
						if let Some(offset) = entry.1.info.get() {
							buf.put_i32(offset.offset as i32 - code_offset as i32);
						} else {
							return Err(InvalidLabelError::UnsetJump{opcode: LOOKUPSWITCH, code_offset});
						}
					}
				} else {
					return Err(InvalidLabelError::UnsetJump{opcode: LOOKUPSWITCH, code_offset});
				}
			},
			Self::TableSwitch(d, l, h, t) => {
				if let Some(i) = d.info.get() {
					let default_offset = i .offset as i32 - code_offset as i32;
					buf.put_u8(TABLESWITCH);
					let padding = 4 - ((code_offset + 1) % 4); // the jvm is a string of odd design descisions
					for _ in 0..padding {
						buf.put_u8(0);
					}
					buf.put_i32(default_offset);
					buf.put_i32(*l);
					buf.put_i32(*h);
					for entry in t {
						if let Some(offset) = entry.info.get() {
							buf.put_i32(offset.offset as i32 - code_offset as i32);
						} else {
							return Err(InvalidLabelError::UnsetJump{opcode: LOOKUPSWITCH, code_offset});
						}
					}
				} else {
					return Err(InvalidLabelError::UnsetJump{opcode: LOOKUPSWITCH, code_offset});
				}
			},
			Nop => {}
		}
		Ok(())
	}
	
	pub fn get_code_length(&self, offset: u16) -> u16 { // current offset is required because SWITCH STATEMENTS SUCK
		match self {
			Basic(_) => 1,
			LoadStore(_, i) if *i <= U8_MAX_U16 => 2,
			LoadStore(_, _) => 3,
			Ldc(_) => 2,
			LdcW(_) => 3,
			Ldc2W(_) => 3,
			Type(_, _) => 3,
			Invoke(_, _) => 3,
			Field(_, _) => 3,
			Self::NewArray(_) => 2,
			MultiNewArray(_, _) => 4,
			BIPush(_) => 2,
			SIPush(_) => 3,
			Self::IInc(i, v) if *i <= U8_MAX_U16 && *v <= I8_MAX_I16 && *v >= I8_MAX_I16 => 3,
			Self::IInc(_, _) => 6,
			Jump(_, _) => 3,
			Self::InvokeDynamic(_) => 5,
			Nop => 0,
			Self::LookupSwitch(_, v) => (4 - ((offset + 1) % 4)) + 8 + (v.len() * 8) as u16,
			Self::TableSwitch(_, l, h, _) => (4 - ((offset + 1) % 4)) + 12 +((h - l + 1) * 4) as u16
 		}
	}
}

const I8_MAX_I16: i16 = i8::MAX as i16;
const I8_MIN_I16: i16 = i8::MIN as i16;
const I8_MAX_I32: i32 = i8::MAX as i32;
const I8_MIN_I32: i32 = i8::MIN as i32;
const I16_MAX_I32: i32 = i16::MAX as i32;
const I16_MIN_I32: i32 = i16::MIN as i32;
const U8_MAX_U16: u16 = u8::MAX as u16;

impl ProcessedOpcode {
	fn process(other: Opcode, pool: &mut ConstantPool) -> Self {
		match other {
			Opcode::Nop => Nop,
			Dup => Basic(DUP),
			Dup2 => Basic(DUP2),
			DupX1 => Basic(DUP_X1),
			DupX2 => Basic(DUP_X2),
			Dup2X1 => Basic(DUP2_X1),
			Dup2X2 => Basic(DUP2_X2),
			Pop => Basic(POP),
			Pop2 => Basic(POP2),
			Swap => Basic(SWAP),
			LCmp => Basic(LCMP),
			FCmpL => Basic(FCMPL),
			FCmpG => Basic(FCMPG),
			DCmpL => Basic(DCMPL),
			DCmpG => Basic(DCMPG),
			ArrayLength => Basic(ARRAYLENGTH),
			AThrow => Basic(ATHROW),
			MonitorEnter => Basic(MONITORENTER),
			MonitorExit => Basic(MONITOREXIT),
			Load(JStackType::Int, 0) => Basic(ILOAD_0),
			Load(JStackType::Int, 1) => Basic(ILOAD_1),
			Load(JStackType::Int, 2) => Basic(ILOAD_2),
			Load(JStackType::Int, 3) => Basic(ILOAD_3),
			Load(JStackType::Int, i) => LoadStore(ILOAD, i),
			Load(JStackType::Long, 0) => Basic(LLOAD_0),
			Load(JStackType::Long, 1) => Basic(LLOAD_1),
			Load(JStackType::Long, 2) => Basic(LLOAD_2),
			Load(JStackType::Long, 3) => Basic(LLOAD_3),
			Load(JStackType::Long, i) => LoadStore(LLOAD, i),
			Load(JStackType::Float, 0) => Basic(FLOAD_0),
			Load(JStackType::Float, 1) => Basic(FLOAD_1),
			Load(JStackType::Float, 2) => Basic(FLOAD_2),
			Load(JStackType::Float, 3) => Basic(FLOAD_3),
			Load(JStackType::Float, i) => LoadStore(FLOAD, i),
			Load(JStackType::Double, 0) => Basic(DLOAD_0),
			Load(JStackType::Double, 1) => Basic(DLOAD_1),
			Load(JStackType::Double, 2) => Basic(DLOAD_2),
			Load(JStackType::Double, 3) => Basic(DLOAD_3),
			Load(JStackType::Double, i) => LoadStore(DLOAD, i),
			Load(JStackType::Reference, 0) => Basic(ALOAD_0),
			Load(JStackType::Reference, 1) => Basic(ALOAD_1),
			Load(JStackType::Reference, 2) => Basic(ALOAD_2),
			Load(JStackType::Reference, 3) => Basic(ALOAD_3),
			Load(JStackType::Reference, i) => LoadStore(ALOAD, i),
			Store(JStackType::Int, 0) => Basic(ISTORE_0),
			Store(JStackType::Int, 1) => Basic(ISTORE_1),
			Store(JStackType::Int, 2) => Basic(ISTORE_2),
			Store(JStackType::Int, 3) => Basic(ISTORE_3),
			Store(JStackType::Int, i) => LoadStore(ISTORE, i),
			Store(JStackType::Long, 0) => Basic(LSTORE_0),
			Store(JStackType::Long, 1) => Basic(LSTORE_1),
			Store(JStackType::Long, 2) => Basic(LSTORE_2),
			Store(JStackType::Long, 3) => Basic(LSTORE_3),
			Store(JStackType::Long, i) => LoadStore(LSTORE, i),
			Store(JStackType::Float, 0) => Basic(FSTORE_0),
			Store(JStackType::Float, 1) => Basic(FSTORE_1),
			Store(JStackType::Float, 2) => Basic(FSTORE_2),
			Store(JStackType::Float, 3) => Basic(FSTORE_3),
			Store(JStackType::Float, i) => LoadStore(FSTORE, i),
			Store(JStackType::Double, 0) => Basic(DSTORE_0),
			Store(JStackType::Double, 1) => Basic(DSTORE_1),
			Store(JStackType::Double, 2) => Basic(DSTORE_2),
			Store(JStackType::Double, 3) => Basic(DSTORE_3),
			Store(JStackType::Double, i) => LoadStore(DSTORE, i),
			Store(JStackType::Reference, 0) => Basic(ASTORE_0),
			Store(JStackType::Reference, 1) => Basic(ASTORE_1),
			Store(JStackType::Reference, 2) => Basic(ASTORE_2),
			Store(JStackType::Reference, 3) => Basic(ASTORE_3),
			Store(JStackType::Reference, i) => LoadStore(ASTORE, i),
			ArrayLoad(JBasicType::Int) => Basic(IALOAD),
			ArrayLoad(JBasicType::Long) => Basic(LALOAD),
			ArrayLoad(JBasicType::Float) => Basic(FALOAD),
			ArrayLoad(JBasicType::Double) => Basic(DALOAD),
			ArrayLoad(JBasicType::Reference) => Basic(AALOAD),
			ArrayLoad(JBasicType::Byte) => Basic(BALOAD),
			ArrayLoad(JBasicType::Boolean) => Basic(BALOAD),
			ArrayLoad(JBasicType::Short) => Basic(SALOAD),
			ArrayLoad(JBasicType::Char) => Basic(CALOAD),
			ArrayStore(JBasicType::Byte) => Basic(BASTORE),
			ArrayStore(JBasicType::Boolean) => Basic(BASTORE),
			ArrayStore(JBasicType::Short) => Basic(SASTORE),
			ArrayStore(JBasicType::Char) => Basic(CASTORE),
			ArrayStore(JBasicType::Int) => Basic(IASTORE),
			ArrayStore(JBasicType::Long) => Basic(LASTORE),
			ArrayStore(JBasicType::Float) => Basic(FASTORE),
			ArrayStore(JBasicType::Double) => Basic(DASTORE),
			ArrayStore(JBasicType::Reference) => Basic(AASTORE),
			Add(t) => match t {
				JNumericStackType::Int => Basic(IADD),
				JNumericStackType::Long => Basic(LADD),
				JNumericStackType::Float => Basic(FADD),
				JNumericStackType::Double => Basic(DADD)
			},
			Sub(t) => match t {
				JNumericStackType::Int => Basic(ISUB),
				JNumericStackType::Long => Basic(LSUB),
				JNumericStackType::Float => Basic(FSUB),
				JNumericStackType::Double => Basic(DSUB)
			}
			Mul(t) => match t {
				JNumericStackType::Int => Basic(IMUL),
				JNumericStackType::Long => Basic(LMUL),
				JNumericStackType::Float => Basic(FMUL),
				JNumericStackType::Double => Basic(DMUL)
			}
			Div(t) => match t {
				JNumericStackType::Int => Basic(IDIV),
				JNumericStackType::Long => Basic(LDIV),
				JNumericStackType::Float => Basic(FDIV),
				JNumericStackType::Double => Basic(DDIV)
			}
			Rem(t) => match t {
				JNumericStackType::Int => Basic(IREM),
				JNumericStackType::Long => Basic(LREM),
				JNumericStackType::Float => Basic(FREM),
				JNumericStackType::Double => Basic(DREM)
			}
			Neg(t) => match t {
				JNumericStackType::Int => Basic(INEG),
				JNumericStackType::Long => Basic(LNEG),
				JNumericStackType::Float => Basic(FNEG),
				JNumericStackType::Double => Basic(DNEG)
			}
			Opcode::IInc(index, constant) => ProcessedOpcode::IInc(index, constant),
			NumConvert(t, u) => match t {
				JNumericStackType::Int => match u {
					JNumericStackType::Int => Nop,
					JNumericStackType::Long => Basic(I2L),
					JNumericStackType::Float => Basic(I2F),
					JNumericStackType::Double => Basic(I2D)
				}
				JNumericStackType::Long => match u {
					JNumericStackType::Int => Basic(L2I),
					JNumericStackType::Long => Nop,
					JNumericStackType::Float => Basic(L2F),
					JNumericStackType::Double => Basic(L2D)
				}
				JNumericStackType::Float => match u {
					JNumericStackType::Int => Basic(F2I),
					JNumericStackType::Long => Basic(F2L),
					JNumericStackType::Float => Nop,
					JNumericStackType::Double => Basic(F2D)
				}
				JNumericStackType::Double => match u {
					JNumericStackType::Int => Basic(D2I),
					JNumericStackType::Long => Basic(D2L),
					JNumericStackType::Float => Basic(D2F),
					JNumericStackType::Double => Nop
				}
			}
			IConvert(JNumericType::Int) => Nop,
			IConvert(JNumericType::Short) => Basic(I2S),
			IConvert(JNumericType::Byte) => Basic(I2B),
			IConvert(JNumericType::Long) => Basic(I2L),
			IConvert(JNumericType::Float) => Basic(I2F),
			IConvert(JNumericType::Double) => Basic(I2D),
			IConvert(JNumericType::Char) => Basic(I2C),
			If(Equal, l) => Jump(IFEQ, l),
			If(NotEqual, l) => Jump(IFNE, l),
			If(GreaterThan, l) => Jump(IFGT, l),
			If(LessThan, l) => Jump(IFLT, l),
			If(GreaterOrEqual, l) => Jump(IFGE, l),
			If(LessOrEqual, l) => Jump(IFLE, l),
			IfICmp(Equal, l) => Jump(IF_ICMPEQ, l),
			IfICmp(NotEqual, l) => Jump(IF_ICMPNE, l),
			IfICmp(GreaterThan, l) => Jump(IF_ICMPGT, l),
			IfICmp(LessThan, l) => Jump(IF_ICMPLT, l),
			IfICmp(GreaterOrEqual, l) => Jump(IF_ICMPGE, l),
			IfICmp(LessOrEqual, l) => Jump(IF_ICMPLE, l),
			IfACmp(RefComparison::Equal, l) => Jump(IF_ACMPEQ, l),
			IfACmp(RefComparison::NotEqual, l) => Jump(IF_ACMPNE, l),
			Goto(l) => Jump(GOTO, l),
			JSr(l) => Jump(JSR, l),
			Ret(i) => LoadStore(RET, i),
			Return(None) => Basic(RETURN),
			Return(Some(JStackType::Int)) => Basic(IRETURN),
			Return(Some(JStackType::Long)) => Basic(LRETURN),
			Return(Some(JStackType::Float)) => Basic(FRETURN),
			Return(Some(JStackType::Double)) => Basic(DRETURN),
			Return(Some(JStackType::Reference)) => Basic(ARETURN),
			Shift(JIntegerStackType::Int, SignedLeft) => Basic(ISHL),
			Shift(JIntegerStackType::Int, SignedRight) => Basic(ISHR),
			Shift(JIntegerStackType::Int, UnsignedRight) => Basic(IUSHR),
			Shift(JIntegerStackType::Long, SignedLeft) => Basic(LSHL),
			Shift(JIntegerStackType::Long, SignedRight) => Basic(LSHR),
			Shift(JIntegerStackType::Long, UnsignedRight) => Basic(LUSHR),
			And(JIntegerStackType::Int) => Basic(IAND),
			And(JIntegerStackType::Long) => Basic(LAND),
			Or(JIntegerStackType::Int) => Basic(IOR),
			Or(JIntegerStackType::Long) => Basic(LOR),
			Xor(JIntegerStackType::Int) => Basic(IXOR),
			Xor(JIntegerStackType::Long) => Basic(LXOR),
			GetField(f) => Field(GETFIELD, pool.field_ref(f)),
			PutField(f) => Field(PUTFIELD, pool.field_ref(f)),
			GetStatic(f) => Field(GETSTATIC, pool.field_ref(f)),
			PutStatic(f) => Field(PUTSTATIC, pool.field_ref(f)),
			InvokeVirtual(m, i) => Invoke(INVOKEVIRTUAL, pool.method_ref(m, i)),
			InvokeStatic(m, i) => Invoke(INVOKESTATIC, pool.method_ref(m, i)),
			InvokeSpecial(m, i) => Invoke(INVOKESPECIAL, pool.method_ref(m, i)),
			InvokeInterface(m) => Invoke(INVOKEINTERFACE, pool.method_ref(m, true)),
			Opcode::InvokeDynamic(c) => ProcessedOpcode::InvokeDynamic(pool.callsite(c)),
			New(c) => Type(NEW, pool.class_ref(c)),
			Opcode::NewArray(JType::Byte) => ProcessedOpcode::NewArray(BYTE),
			Opcode::NewArray(JType::Boolean) => ProcessedOpcode::NewArray(BOOLEAN),
			Opcode::NewArray(JType::Short) => ProcessedOpcode::NewArray(SHORT),
			Opcode::NewArray(JType::Char) => ProcessedOpcode::NewArray(CHAR),
			Opcode::NewArray(JType::Int) => ProcessedOpcode::NewArray(INT),
			Opcode::NewArray(JType::Float) => ProcessedOpcode::NewArray(FLOAT),
			Opcode::NewArray(JType::Long) => ProcessedOpcode::NewArray(LONG),
			Opcode::NewArray(JType::Double) => ProcessedOpcode::NewArray(DOUBLE),
			Opcode::NewArray(JType::Class(c)) => ProcessedOpcode::Type(ANEWARRAY, pool.class_ref(c)),
			Opcode::NewArray(JType::Array(a)) => ProcessedOpcode::Type(ANEWARRAY, pool.array_type_ref(JType::Array(a))),
			Opcode::MultiANewArray(t, d) => ProcessedOpcode::MultiNewArray(pool.array_type_ref(t.into()), d),
			CheckCast(c) => Type(CHECKCAST, pool.class_ref(c)),
			InstanceOf(c) => Type(INSTANCEOF, pool.class_ref(c)),
			IfNull(l) => Jump(IFNULL, l),
			IfNonNull(l) => Jump(IFNONNULL, l),
			//oh god oh fuck.
			Const(JConst::Byte(v)) => match v {
				-1 => Basic(ICONST_M1),
				0 => Basic(ICONST_0),
				1 => Basic(ICONST_1),
				2 => Basic(ICONST_2),
				3 => Basic(ICONST_3),
				4 => Basic(ICONST_4),
				5 => Basic(ICONST_5),
				b => BIPush(b)
			}
			Const(JConst::Short(v)) => match v {
				-1 => Basic(ICONST_M1),
				0 => Basic(ICONST_0),
				1 => Basic(ICONST_1),
				2 => Basic(ICONST_2),
				3 => Basic(ICONST_3),
				4 => Basic(ICONST_4),
				5 => Basic(ICONST_5),
				I8_MIN_I16..=I8_MAX_I16 => BIPush(v as i8),
				b => SIPush(b)
			}
			Const(JConst::Char(v)) => match v as i16 {
				-1 => Basic(ICONST_M1),
				0 => Basic(ICONST_0),
				1 => Basic(ICONST_1),
				2 => Basic(ICONST_2),
				3 => Basic(ICONST_3),
				4 => Basic(ICONST_4),
				5 => Basic(ICONST_5),
				I8_MIN_I16..=I8_MAX_I16 => BIPush(v as i8),
				b => SIPush(b)
			}
			Const(JConst::Int(v)) => match v {
				-1 => Basic(ICONST_M1),
				0 => Basic(ICONST_0),
				1 => Basic(ICONST_1),
				2 => Basic(ICONST_2),
				3 => Basic(ICONST_3),
				4 => Basic(ICONST_4),
				5 => Basic(ICONST_5),
				I8_MIN_I32..=I8_MAX_I32 => BIPush(v as i8),
				I16_MIN_I32..=I16_MAX_I32 => SIPush(v as i16),
				b => get_ldc(pool.int_const(b))
			}
			
			Const(JConst::Boolean(true)) => Basic(ICONST_1),
			Const(JConst::Boolean(false)) => Basic(ICONST_0),
			Const(JConst::Long(v)) => match v {
				0 => Basic(LCONST_0),
				1 => Basic(LCONST_1),
				b => Ldc2W(pool.long_const(b))
			}
			Const(JConst::Float(v)) => match v {
				0. => Basic(FCONST_0),
				1. => Basic(FCONST_1),
				2. => Basic(FCONST_2),
				b => get_ldc(pool.float_const(b))
			}
			Const(JConst::Double(v)) => match v {
				0. => Basic(DCONST_0),
				1. => Basic(DCONST_1),
				b => Ldc2W(pool.double_const(b))
			}
			Const(JConst::Class(c)) => get_ldc(pool.class_ref(c)),
			Const(JConst::MethodType(t)) => get_ldc(pool.method_type(t)),
			Const(JConst::Handle(h)) => get_ldc(pool.handle(h)),
			Const(JConst::Str(s)) => get_ldc(pool.string_const(s)),
			Const(JConst::Null) => Basic(ACONST_NULL),
			Opcode::LookupSwitch(l, m) => Self::LookupSwitch(l, m),
			Opcode::TableSwitch(d, l, h, t) => Self::TableSwitch(d, l, h, t)
		}
	}
}

fn get_ldc(pool_index: u16) -> ProcessedOpcode {
	if pool_index <= u8::MAX as u16 {
		Ldc(pool_index as u8)
	} else {
		LdcW(pool_index)
	}
}
