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
impl<T> HxObject for BaseIter<T> {
}
impl HxObject for Clone {
}
impl HxObject for os::Pipe {
}
impl HxObject for path::Path {
}
impl HxObject for io::ReaderUtil {
}
impl HxObject for io::Reader {
}
impl HxObject for io::Writer {
}
