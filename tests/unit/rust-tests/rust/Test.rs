mod lib;
pub struct Test {
	value: Option<i32>
	
}
pub impl Test {
	priv fn assert(v: bool, msg: Option<@str>) -> () {
		if (!v) {
			fail!(msg);
		}
		}
	
	pub fn main() -> () {
		let mut c: Option<@Test> = Test::new();
		c.unwrap().value = Some((67i32 % 2i32));
		c.unwrap().value = Some((((c.unwrap().value / Some(0.2f64))) as i32));
		Test::assert((c.unwrap().value == Some(5i32)),None);
	}
	
	pub fn new() -> Option<@Test> {
		let mut self = Test {value: None};;
		self.value = None;
		return Some(@self);
	}
	
}
impl lib::HxObject for Test {
	pub fn toString(&self) -> Option<@str> {
		return Some(@"Test");
	}
}
