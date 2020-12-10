use crate::opcodes::JConst;
use crate::elementrefs::MethodType;

#[macro_export]
macro_rules! jvm_bytecode {
	// constants
	($codevis:ident; Const $val:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Const($val.into()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Const $typ:ident $val:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Const(crate::opcodes::JConst::$typ($val)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; GetStatic $field:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::GetStatic($field));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	// fields
	($codevis:ident; PutStatic $field:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(Opcode::PutStatic($field));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; GetField $field:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(Opcode::GetField($field));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; PutField $field:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(Opcode::PutField($field));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	// invokes
	($codevis:ident; InvokeVirtual $method:expr, $itf:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::InvokeVirtual($method, $itf));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; InvokeSpecial $method:expr, $itf:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(Opcode::InvokeSpecial($method, $itf));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; InvokeStatic $method:expr, $itf:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::InvokeStatic($method, $itf));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; InvokeInterface $method:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(Opcode::InvokeInterface($method));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	// returns
	($codevis:ident; Return $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(Opcode::Return(Some(jtype!($typ))));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Return; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Return(None));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	// loads, stores
	($codevis:ident; Load $typ:tt $idx:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Load(jtype!($typ), $idx));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Store $typ:tt $idx:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Store(jtype!($typ), $idx));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; ArrayLoad $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::ArrayLoad(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; ArrayStore $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::ArrayStore(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	// flow
	($codevis:ident; Goto $label:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Goto($label.clone()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; If $cmp:tt, $lab:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::If(comparison!($cmp), $lab.clone()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; IfICmp $cmp:tt, $lab:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::IfICmp(comparison!($cmp), $lab.clone()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; IfACmp $cmp:tt, $lab:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::IfACmp(r_comparison!($cmp), $lab.clone()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; JSr $label:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::JSr($label.clone()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Ret $jmp:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Ret($jmp));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; IfNull $label:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::IfNull($label.clone()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; IfNonNull $label:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::IfNonNull($label.clone()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	
	
	// math
	($codevis:ident; Add $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Add(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Sub $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Sub(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Mul $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Mul(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Div $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Div(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Neg $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Neg(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Rem $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Rem(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; IInc $idx:expr, $amt:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::IInc($idx, $amt));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; And $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::And(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Or $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Or(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Xor $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Xor(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; Shift $typ:tt $shfttype:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Shift(jtype!($typ), shift_type!($shfttype)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	// converts
	($codevis:ident; Convert $ty1:tt $ty2:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::Convert(jtype!($ty1), jtype!($ty2)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; IConvert $typ:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::IConvert(jtype!($typ)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	// labels
	($codevis:ident; Label $label:expr; $($tail:tt)*) => {
		$codevis.visit_label($label.clone());
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; InitLabel $label:ident; $($tail:tt)*) => {
		let $label = crate::opcodes::Label::new();
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	// misc
	($codevis:ident; New $class:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::New($class.into()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; NewArray $type:tt; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::NewArray(jtype!($type)));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; MultiANewArray $type:tt $qty:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::MultiANewArray(jtype!($type), $qty));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; CheckCast $class:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::CheckCast($class.into()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident; InstanceOf $class:expr; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::InstanceOf($class.into()));
		jvm_bytecode!{$codevis; $($tail)*}
	};
	
	// fallback for basic opcodes so i don't have to do them manually
	($codevis:ident; $opcode:ident; $($tail:tt)*) => {
		$codevis.visit_opcode(crate::opcodes::Opcode::$opcode);
		jvm_bytecode!{$codevis; $($tail)*}
	};
	($codevis:ident;) => {}
}

#[macro_export]
macro_rules! field_ref {
	($owner:expr , $name:ident $typ:tt) => {
		crate::elementrefs::FieldRef::new($owner.into(), stringify!($name).into(), jtype![$typ])
	}
}

#[macro_export]
macro_rules! method_ref {
	($owner:expr , $name:ident ($($types:tt),*) $ret:tt) => {
		crate::elementrefs::MethodRef::new($owner.into(), stringify!($name).into(), method_type![($($types),*) $ret])
	}
}

#[macro_export]
macro_rules! method_type {
	(($($types:tt),*)$ret:tt) => {
		crate::elementrefs::MethodType::new(vec![$(jtype![$types]),*], jtype_v!($ret))
	}
}

#[macro_export]
macro_rules! jtype {
	(I) => {crate::types::Int.into()};
	(int) => {crate::types::Int.into()};
	(B) => {crate::types::Byte.into()};
	(byte) => {crate::types::Byte.into()};
	(Z) => {crate::types::Boolean.into()};
	(boolean) => {crate::types::Boolean.into()};
	(J) => {crate::types::Long.into()};
	(long) => {crate::types::Long.into()};
	(C) => {crate::types::Char.into()};
	(char) => {crate::types::Char.into()};
	(S) => {crate::types::Short.into()};
	(short) => {crate::types::Short.into()};
	(F) => {crate::types::Float.into()};
	(float) => {crate::types::Float.into()};
	(D) => {crate::types::Double.into()};
	(double) => {crate::types::Double.into()};
	([$typ:tt]) => {
		crate::types::Array(jtype!($typ)).into()
	};
	($class:expr) => {
		crate::types::Class(String::from($class)).into()
	} 
}

#[macro_export]
macro_rules! jtype_v {
	(V) => {None};
	(void) => {None};
	($other:tt) => {Some(jtype!($other))};
}

#[macro_export]
macro_rules! access {
	($typ:ident: $($acc:ident)*) => {
		$typ {
			$($acc: true,)*
			..Default::default()
		}
	}
}

#[macro_export]
macro_rules! comparison {
	(==) => {crate::opcodes::RefComparison::Equal.into()};
	(>=) => {crate::opcodes::Comparison::GreaterOrEqual};
	(>) => {crate::opcodes::Comparison::GreaterThan};
	(<=) => {crate::opcodes::Comparison::LessOrEqual};
	(<) => {crate::opcodes::Comparison::LessThan};
	(!=) => {crate::opcodes::RefComparison::NotEqual.into()};
	($e:expr) => {$e};
}

#[macro_export]
macro_rules! shift_type {
	(<<) => {crate::opcodes::ShiftType::SignedLeft};
	(>>) => {crate::opcodes::ShiftType::SignedRight};
	(>>>) => {crate::opcodes::ShiftType::UnsignedRight};
	($e:expr) => {$e};
}

impl From<&str> for JConst {
	fn from(other: &str) -> Self {
		JConst::Str(other.into())
	}
}

impl From<i32> for JConst {
	fn from(other: i32) -> Self {
		JConst::Int(other)
	}
}

impl From<f32> for JConst {
	fn from(other: f32) -> Self {
		JConst::Float(other)
	}
}

impl From<MethodType> for JConst {
	fn from(other: MethodType) -> Self {
		JConst::MethodType(other)
	}
}
