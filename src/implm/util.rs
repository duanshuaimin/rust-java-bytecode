use bytes::BytesMut;
use std::ops::Deref;

pub fn write_u32(buf: &mut BytesMut, offset: usize, v: u32) {
	let bytes = v.to_be_bytes();
	buf[offset] = bytes[0];
	buf[offset + 1] = bytes[1];
	buf[offset + 2] = bytes[2];
	buf[offset + 3] = bytes[3];
}

pub enum MaybeRef<'a, T> {
	Owned(T),
	Ref(&'a T)
}

impl<T> Deref for MaybeRef<'_, T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		match self {
			Self::Owned(t) => &t,
			Self::Ref(t) => t
		}
	}
}
