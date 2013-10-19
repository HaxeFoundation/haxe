pub trait HxEnum {
}
pub trait HxObject {
	fn toString(&self) -> Option<~str>;
}
impl HxObject for ~Clone {
	fn toString(&self) -> Option<~str> {
		return Some(~"Clone: { }");
	}
}
impl<T> HxObject for ~Iterator<T> {
	fn toString(&self) -> Option<~str> {
		return Some(~"Iterator: { }");
	}
}
impl HxObject for ~os::Pipe {
	fn toString(&self) -> Option<~str> {
		return Some(~"Pipe: { pout: "+self.pout.toString()+", pin: "+self.pin.toString()+"}");
	}
}
impl HxObject for ~std::path::Path {
	fn toString(&self) -> Option<~str> {
		return Some(~"Path: { components: "+self.components.toString()+", is_absolute: "+self.is_absolute.toString()+"}");
	}
}
impl HxObject for ~std::io::ReaderUtil {
	fn toString(&self) -> Option<~str> {
		return Some(~"ReaderUtil: { }");
	}
}
impl HxObject for ~std::io::Reader {
	fn toString(&self) -> Option<~str> {
		return Some(~"Reader: { }");
	}
}
impl HxObject for ~std::io::Writer {
	fn toString(&self) -> Option<~str> {
		return Some(~"Writer: { }");
	}
}
impl HxObject for i8 {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for i16 {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for i32 {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for i64 {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for int {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for float {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for f32 {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for f64 {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}
impl HxObject for ~str {
	fn toString(&self) -> Option<~str> {
		return Some(self.to_str());
	}
}

