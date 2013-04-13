mod HxObject;
mod HxEnum;
pub struct Test {
	value: Option<@T>
}
pub impl Test {
	pub fn main() -> () {
		Test.new(78.533567f32);
		io::println(Some(@"898687"));
		io::println(Some(@"Hello, world!"));
		io::println(Std::string(f32::floor(88621698.2703f32)));
	}
	pub fn new(val: Option<@T>) -> Option<@Test> {
		let mut self = Option<@Test> {value: None}
		(Std::unwrap(self)).value = val;
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
	pub fn __set_field(&self, field:&str, value:&Option<&HxObject>) {
		match(field) {
			"value" => self.value = value,
			_ => None
		}
	}
}
