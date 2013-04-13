mod HxObject;
mod HxEnum;
pub struct Test<T> {
	value: Option<@T>
}
pub impl<T> Test<T> {
	pub fn main() -> () {
		Test.new(78.533567f32);
		Some(@"898687");
	}
	pub fn new(val: Option<@T>) -> Option<@Test> {
		let mut self = Option<@Test> {value: None}
		(rust::Lib::unwrap(self)).value = val;
		return @Some(self);
	}
}
impl HxObject for Option<@Test> {
	pub fn __get_field(&self, &field:str) {
		return match(field) {
			"value" => self.value,
			_ => None
		}
	}
	pub fn __set_field(&mut self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"value" => self.value = value,
			_ => None
		}
	}
}
