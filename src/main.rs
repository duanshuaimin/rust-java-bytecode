
pub mod opcodes;
pub mod elementrefs;
pub mod types;
pub mod visitor;
pub mod access;
pub mod macros;
mod implm;
use visitor::writer::ClassWriterFactory;
use visitor::*;
use visitor::util::frames::*;
use types::*;
use access::*;
use std::fs::File;
use std::io::prelude::*;

pub fn main() {
	let cvf = FrameComputingClassVisitorFactory::new(ClassWriterFactory::new());
	let mut cw = cvf.create(access!(ClassAccess: a_public a_super), "Main".into(), "java/lang/Object".into(), vec![], 52, 0);//ClassWriter::visit(access!(ClassAccess: a_public a_super), "Main".into(), "java/lang/Object".into(), vec![], 49, 0);
	let mv = cw.visit_method(access!(MethodAccess: a_public a_static), "main".into(), method_type!((["java/lang/String"])void));
	let mut cv = mv.unwrap().visit_code().unwrap();
	jvm_bytecode! { cv;
		InitLabel l0;
		Const 1;
		Store int 1;
		Label l0;
		GetStatic field_ref!("java/lang/System", out "java/io/PrintStream");
		Load int 1;
		InvokeVirtual method_ref!("java/io/PrintStream", println(int)void), false;
		Load int 1;
		Const 5;
		Mul int;
		Store int 1;
		Const Long 100;
		InvokeStatic method_ref!("java/lang/Thread", sleep(long)void), false;
		InitLabel l1;
		Load int 1;
		Const (5 as i32).pow(10);
		IfICmp >=, l1;
		Goto l0;
		Label l1;
		Return;
	}
	//cv.visit_frame(StackFrame::Full(lhead, vec![Int.into()], vec![]));
	//cv.visit_frame(StackFrame::Full(l0, vec![jtype!(["java/lang/String"]), Int.into()], vec![]));
	//cv.visit_frame(StackFrame::Full(l1, vec![], vec![]));
	cv.visit_maxs(5, 5);
	cv.visit_end();
	let mut f = File::create("Main.class").unwrap();
	let bytes = cw.serialize();
	f.write_all(&bytes[..]);
}

