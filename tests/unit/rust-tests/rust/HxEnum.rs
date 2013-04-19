pub trait HxEnum {
	pub fn __name() -> Option<@str>;
	pub fn __get_index(ind:i32) -> Self;
	pub fn __parameters(&self) -> Option<~[@HxObject]>;
	pub fn __index(&self) -> i32;
	pub fn __constructor(&self) -> @str;
}
