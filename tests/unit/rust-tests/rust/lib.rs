use os::Pipe;
use io::ReaderUtil;
use io::Writer;
use io::Reader;
use path::Path;
mod BaseIter;
mod Clone;
pub trait HxEnum {
	pub fn __name() -> Option<@str>;
	pub fn __get_index(ind:i32) -> Self;
	pub fn __parameters(&self) -> Option<~[@HxObject]>;
	pub fn __index(&self) -> i32;
	pub fn __constructor(&self) -> @str;
}
pub trait HxObject {
	pub fn __name() -> Option<@str>;
	pub fn __field(&self, field:@str) -> Option<HxObject>;
	pub fn __set_field(&mut self, field:@str, value:Option<@HxObject>) -> Option<@HxObject>;
	pub fn toString(&self) -> @str;
}
impl ToStr for HxObject {
	pub fn to_str(&self) -> ~str {
		return self.toString();
	}
}
/*Package BaseIter => no*/impl HxObject for BaseIter<T> {
}
/*Package Clone => no*/impl HxObject for Clone {
}
/*Package Pipe => no*/impl HxObject for os::Pipe {
}
/*Package Path => no*/impl HxObject for path::Path {
}
/*Package ReaderUtil => no*/impl HxObject for io::ReaderUtil {
}
/*Package Reader => no*/impl HxObject for io::Reader {
}
/*Package Writer => no*/impl HxObject for io::Writer {
}
