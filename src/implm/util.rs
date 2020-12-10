use bytes::BytesMut;

pub fn write_u32(buf: &mut BytesMut, offset: usize, v: u32) {
	let bytes = v.to_be_bytes();
	buf[offset] = bytes[0];
	buf[offset + 1] = bytes[1];
	buf[offset + 2] = bytes[2];
	buf[offset + 3] = bytes[3];
}
