#[derive(Default)]
pub struct ClassAccess {
	pub a_public: bool,
	pub a_protected: bool,
	pub a_private: bool,
	pub a_final: bool,
	pub a_super: bool,
	pub a_interface: bool,
	pub a_abstract: bool,
	pub a_synthetic: bool,
	pub a_annotation: bool,
	pub a_enum: bool,
	pub a_module: bool
}

macro_rules! bitflag_pack {
	($o:ident $($x:ident: $y:expr),*) => {
		{
			let mut result = 0;
			$(if $o.$x {result |= $y})*
			result
		}
	}
}

impl ClassAccess {
	pub fn new() -> ClassAccess {
		ClassAccess {
			a_public: false,
			a_protected: false,
			a_private: false,
			a_final: false,
			a_super: false,
			a_interface: false,
			a_abstract: false,
			a_synthetic: false,
			a_annotation: false,
			a_enum: false,
			a_module: false
		}
	}
	
	pub fn as_bitflag(&self) -> u16 {
		bitflag_pack!{ self
			a_public: 0x0001,
			a_protected: 0x0004,
			a_private: 0x0002,
			a_final: 0x0010,
			a_super: 0x0020,
			a_interface: 0x0200,
			a_abstract: 0x0400,
			a_synthetic: 0x1000,
			a_annotation: 0x2000,
			a_enum: 0x4000,
			a_module: 0x8000
		}
	}
}

#[derive(Default)]
pub struct MethodAccess {
	pub a_public: bool,
	pub a_protected: bool,
	pub a_private: bool,
	pub a_final: bool,
	pub a_static: bool,
	pub a_synchronized: bool,
	pub a_bridge: bool,
	pub a_varargs: bool,
	pub a_native: bool,
	pub a_abstract: bool,
	pub a_strict: bool,
	pub a_synthetic: bool,
	pub a_mandated: bool
}

impl MethodAccess {
	pub fn new() -> MethodAccess {
		MethodAccess {
			a_public: false,
			a_protected: false,
			a_private: false,
			a_final: false,
			a_static: false,
			a_synchronized: false,
			a_bridge: false,
			a_varargs: false,
			a_native: false,
			a_abstract: false,
			a_strict: false,
			a_synthetic: false,
			a_mandated: false
		}
	}
	pub fn as_bitflag(&self) -> u16 {
		bitflag_pack!{ self
			a_public: 0x0001,
			a_protected: 0x0004,
			a_private: 0x0002,
			a_final: 0x0010,
			a_static: 0x0008,
			a_synchronized: 0x0020,
			a_bridge: 0x0040,
			a_varargs: 0x0080,
			a_native: 0x0100,
			a_abstract: 0x0400,
			a_strict: 0x0800,
			a_synthetic: 0x1000,
			a_mandated: 0x8000
		}
	}
}

#[derive(Default)]
pub struct FieldAccess {
	pub a_public: bool,
	pub a_protected: bool,
	pub a_private: bool,
	pub a_final: bool,
	pub a_static: bool,
	pub a_volatile: bool,
	pub a_transient: bool,
	pub a_synthetic: bool,
	pub a_enum: bool,
	pub a_mandated: bool
}

impl FieldAccess {
	pub fn new() -> FieldAccess {
		FieldAccess {
			a_public: false,
			a_protected: false,
			a_private: false,
			a_final: false,
			a_static: false,
			a_volatile: false,
			a_transient: false,
			a_synthetic: false,
			a_enum: false,
			a_mandated: false
		}
	}
	
	pub fn as_bitflag(&self) -> u16 {
		bitflag_pack!{ self
			a_public: 0x0001,
			a_protected: 0x0004,
			a_private: 0x0002,
			a_final: 0x0010,
			a_static: 0x0008,
			a_volatile: 0x0040,
			a_transient: 0x0080,
			a_synthetic: 0x1000,
			a_enum: 0x4000,
			a_mandated: 0x8000
		}
	}
}
