extern mod std;
pub trait HxEnum {
}
pub trait HxObject {
	pub fn toString(&self) -> Option<~str>;
}
impl<T> HxObject for BaseIter<T> {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"BaseIter")
	}
}
impl HxObject for Clone {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Clone")
	}
}
impl HxObject for os::Pipe {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Pipe")
	}
}
impl HxObject for path::Path {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Path")
	}
}
impl HxObject for io::ReaderUtil {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"ReaderUtil")
	}
}
impl HxObject for io::Reader {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Reader")
	}
}
impl HxObject for io::Writer {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Writer")
	}
}
impl HxObject for i8 {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for i16 {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for i32 {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for i64 {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for int {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for float {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for f32 {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for f64 {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for ~str {
	pub fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}

