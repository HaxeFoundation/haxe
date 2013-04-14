mod HxEnum;
pub trait HxEnum {
	pub fn __get_index(ind:i32) -> Self;
	pub fn __index(&self) -> i32;
}
