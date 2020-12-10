use bytes::{BytesMut, BufMut, Bytes};
use std::{
	collections::HashMap,
	hash::{Hash, Hasher},
	rc::Rc,
	cell::RefCell
};
use crate::{
	elementrefs::*,
	types::JType,
	visitor::writer::CodeWriter,
	visitor::StackFrame
};
//use crate::prelude::*;

pub mod consts;
pub mod util;
use consts::cpooltypes::*;
use util::write_u32;

pub struct ConstantPool {
	buf: BytesMut,
	index_map: HashMap<InternalConstantPoolEntry, u16>,
	index: u16,
	bootstrap_methods: Rc<RefCell<BootstrapMethodPool>>
}

impl ConstantPool  {
	pub fn new(bootstrap_methods: Rc<RefCell<BootstrapMethodPool>>) -> Self {
		ConstantPool {
			buf: BytesMut::new(),
			index: 1,
			index_map: HashMap::new(),
			bootstrap_methods
		}
	}
	
	pub fn class_ref(&mut self, class: ClassRef) -> u16 {
		let ptr = self.utf8(class);
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::Class(ptr)).or_insert_with(|| {
			buf.put_u8(C_CLASS);
			buf.put_u16(ptr);
			inc_index(idx)
		})
	}
	
	pub fn field_ref(&mut self, field: FieldRef) -> u16 {
		let name_type_ptr = self.name_and_type(field.get_name().clone(), field.get_type().descriptor());
		let class_ptr = self.class_ref(field.get_class().clone());
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::FieldRef(class_ptr, name_type_ptr)).or_insert_with(|| {
			buf.put_u8(C_FIELDREF);
			buf.put_u16(class_ptr);
			buf.put_u16(name_type_ptr);
			inc_index(idx)
		})
	}
	
	pub fn array_type_ref(&mut self, t: JType) -> u16 {
		self.class_ref(t.descriptor())
	}
	
	pub fn method_ref(&mut self, method: MethodRef, interface: bool) -> u16 {
		let name_type_ptr = self.name_and_type(method.get_name().clone(), method.get_type().descriptor());
		let class_ptr = self.class_ref(method.get_class().clone());
		let entry = if interface {
			InternalConstantPoolEntry::IfaceMethodRef(class_ptr, name_type_ptr)
		} else {
			InternalConstantPoolEntry::MethodRef(class_ptr, name_type_ptr)
		};
		let tag = entry.get_tag();
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(entry).or_insert_with(|| {
			buf.put_u8(tag);
			buf.put_u16(class_ptr);
			buf.put_u16(name_type_ptr);
			inc_index(idx)
		})
	}
	
	pub fn callsite(&mut self, callsite: CallSite) -> u16 {
		let method_ptr = self.handle(Handle::Method(callsite.bootstrap));
		let mut args = Vec::new();
		for arg in callsite.args {
			args.push(self.callsite_const(arg))
		}
		let bootstrap = BootstrapMethod {
			args, method: method_ptr
		};
		
		let bootstrap_ptr = self.bootstrap_methods.borrow_mut().method(bootstrap);
		let name_type_ptr = self.name_and_type(callsite.name, callsite.ty.descriptor());
		
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::InvokeDynamic(bootstrap_ptr, name_type_ptr)).or_insert_with(|| {
			buf.put_u8(C_INVOKEDYNAMIC);
			buf.put_u16(bootstrap_ptr);
			buf.put_u16(name_type_ptr);
			inc_index(idx)
		})
	}
	
	pub fn int_const(&mut self, int: i32) -> u16 {
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::Integer(int)).or_insert_with(|| {
			buf.put_u8(C_INTEGER);
			buf.put_i32(int);
			inc_index(idx)
		})
	}
	
	pub fn long_const(&mut self, long: i64) -> u16 {
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::Long(long)).or_insert_with(|| {
			buf.put_u8(C_LONG);
			buf.put_i64(long);
			let index = *idx;
			*idx += 2;
			index
		})
	}
	
	pub fn float_const(&mut self, float: f32) -> u16 {
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::Float(HF32(float))).or_insert_with(|| {
			buf.put_u8(C_FLOAT);
			buf.put_f32(float);
			inc_index(idx)
		})
	}
	
	pub fn double_const(&mut self, double: f64) -> u16 {
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::Double(HF64(double))).or_insert_with(|| {
			buf.put_u8(C_DOUBLE);
			buf.put_f64(double);
			let index = *idx;
			*idx += 2;
			index
		})
	}
	
	pub fn method_type(&mut self, t: MethodType) -> u16 {
		let ptr = self.utf8(t.descriptor());
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::MethodType(ptr)).or_insert_with(|| {
			buf.put_u8(C_METHODTYPE);
			buf.put_u16(ptr);
			inc_index(idx)
		})
	}
	
	pub fn handle(&mut self, h: Handle) -> u16 {
		let ty = h.get_index();
		let ptr = match h {
			Handle::Field(fh) => self.field_ref(fh.0),
			Handle::Method(mh) => self.method_ref(mh.0, mh.1 == MethodHandleAction::InvokeInterface)
		};
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::MethodHandle(ty, ptr)).or_insert_with(|| {
			buf.put_u8(C_METHODHANDLE);
			buf.put_u8(ty);
			buf.put_u16(ptr);
			inc_index(idx)
		})
	}
	
	pub fn string_const(&mut self, string: String) -> u16 {
		let ptr = self.utf8(string);
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::Str(ptr)).or_insert_with(|| {
			buf.put_u8(C_STRING);
			buf.put_u16(ptr);
			inc_index(idx)
		})
	}
	
	pub fn utf8(&mut self, utf8: String) -> u16 {
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::Utf8(utf8.clone())).or_insert_with(|| {
			buf.put_u8(C_UTF8);
			let bytes = utf8.as_bytes();
			buf.put_u16(bytes.len() as u16);
			buf.put_slice(bytes);
			inc_index(idx)
		})
	}
	
	pub fn name_and_type(&mut self, name: String, desc: String) -> u16 {
		let (nameptr, descptr) = (self.utf8(name), self.utf8(desc));
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.index_map.entry(InternalConstantPoolEntry::NameAndType(nameptr, descptr)).or_insert_with(|| {
			buf.put_u8(C_NAME_AND_TYPE);
			buf.put_u16(nameptr);
			buf.put_u16(descptr);
			inc_index(idx)
		})
	}
	
	pub fn serialize(&self) -> Bytes {
		self.buf.clone().freeze()
	}
	
	pub fn get_index(&self) -> u16 {
		self.index
	}
	
	fn callsite_const(&mut self, constant: CallSiteConstant) -> u16 {
		match constant {
			CallSiteConstant::Int(i) => self.int_const(i),
			CallSiteConstant::Long(l) => self.long_const(l),
			CallSiteConstant::Float(f) => self.float_const(f),
			CallSiteConstant::Double(d) => self.double_const(d),
			CallSiteConstant::Class(c) => self.class_ref(c),
			CallSiteConstant::Handle(h) => self.handle(h),
			CallSiteConstant::Str(s) => self.string_const(s)
		}
	}
}

pub struct BootstrapMethodPool {
	buf: BytesMut,
	methods: HashMap<BootstrapMethod, u16>,
	index: u16
}

impl BootstrapMethodPool {
	pub fn method(&mut self, m: BootstrapMethod) -> u16 {
		let buf = &mut self.buf;
		let idx = &mut self.index;
		*self.methods.entry(m.clone()).or_insert_with(|| {
			buf.put_u16(m.method);
			buf.put_u16(m.args.len() as u16);
			for arg in m.args {
				buf.put_u16(arg);
			}
			inc_index(idx)
		})
	}
	
	pub fn new() -> Self {
		BootstrapMethodPool {
			buf: BytesMut::new(),
			methods: HashMap::new(),
			index: 0
		}
	}
}

impl Attribute for Rc<RefCell<BootstrapMethodPool>> {
	fn serialize(&self, buf: &mut BytesMut) {
		buf.put_u16(self.borrow().index);
		buf.put(self.borrow().buf.clone());
	}
}

#[derive(Eq, Hash, PartialEq, Debug, Clone)]
pub struct BootstrapMethod {
	method: u16,
	args: Vec<u16>
}

fn inc_index(index: &mut u16) -> u16 {
	*index += 1;
	*index - 1
}

#[derive(PartialEq, Eq, Hash)]
enum InternalConstantPoolEntry {
	Class(u16),
	FieldRef(u16, u16),
	MethodRef(u16, u16),
	IfaceMethodRef(u16, u16),
	Str(u16),
	Integer(i32),
	Float(HF32),
	Long(i64),
	Double(HF64),
	NameAndType(u16, u16),
	Utf8(String),
	MethodHandle(u8, u16),
	MethodType(u16),
	InvokeDynamic(u16, u16),
}

impl InternalConstantPoolEntry {
	fn get_tag(&self) -> u8 {
		match &self {
			InternalConstantPoolEntry::Utf8(_) => C_UTF8,
			InternalConstantPoolEntry::Integer(_) => C_INTEGER,
			InternalConstantPoolEntry::Float(_) => C_FLOAT,
			InternalConstantPoolEntry::Long(_) => C_LONG,
			InternalConstantPoolEntry::Double(_) => C_DOUBLE,
			InternalConstantPoolEntry::Class(_) => C_CLASS,
			InternalConstantPoolEntry::Str(_) => C_STRING,
			InternalConstantPoolEntry::FieldRef(_, _) => C_FIELDREF,
			InternalConstantPoolEntry::MethodRef(_, _) => C_METHODREF,
			InternalConstantPoolEntry::IfaceMethodRef(_, _) => C_INTERFACE_METHODREF,
			InternalConstantPoolEntry::NameAndType(_, _) => C_NAME_AND_TYPE,
			InternalConstantPoolEntry::MethodHandle(_, _) => C_METHODHANDLE,
			InternalConstantPoolEntry::MethodType(_) => C_METHODTYPE,
			InternalConstantPoolEntry::InvokeDynamic(_, _) => C_INVOKEDYNAMIC 
		}
	}
}

#[derive(PartialEq)]
struct HF32(f32);
#[derive(PartialEq)]
struct HF64(f64);
impl Eq for HF32 {}
impl Eq for HF64 {}
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for HF32 {
	fn hash<T: Hasher>(&self, hasher: &mut T) {
		let i: i32 = self.0.to_bits() as i32;
		i.hash(hasher)
	}
}
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for HF64 {
	fn hash<T: Hasher>(&self, hasher: &mut T) {
		let i: i64 = self.0.to_bits() as i64;
		i.hash(hasher)
	}
}

pub struct AttributePool {
	attribs: HashMap<String, (AttrEnum, u16)>
}

impl AttributePool {
	pub fn new() -> Self {
		AttributePool {
			attribs: HashMap::new()
		}
	}
	
	pub fn put(&mut self, name: String, attr: AttrEnum, pool: &mut ConstantPool) -> &mut AttrEnum {
		&mut self.attribs.entry(name.clone()).or_insert((attr, pool.utf8(name))).0
	}
	
	pub fn len(&self) -> usize {
		self.attribs.len()
	}
	
	pub fn serialize(&self) -> Bytes {
		let mut buf = BytesMut::new();
		buf.put_u16(self.len() as u16);
		for (attr, name) in self.attribs.values() {
			buf.put_u16(*name);
			let offset = buf.len();
			buf.put_u32(0);
			attr.serialize(&mut buf);
			let rustthonk = &mut buf;
			write_u32(rustthonk, offset, (rustthonk.len() - offset - 4) as u32);
		}
		buf.freeze()
	}
}

pub enum AttrEnum {
	Dyn(Box<dyn Attribute>),
	Code(Rc<RefCell<CodeWriter>>),
	Frames(Rc<RefCell<Vec<StackFrame>>>)
}

impl Attribute for AttrEnum {
	fn serialize(&self, buf: &mut BytesMut) {
		match self {
			Self::Dyn(d) => d.serialize(buf),
			Self::Code(c) => c.borrow().serialize(buf),
			Self::Frames(v) => v.borrow().serialize(buf)
		}
	}
}

pub trait Attribute {
	fn serialize(&self, buf: &mut BytesMut);
}
