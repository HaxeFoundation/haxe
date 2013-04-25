mod lib;
pub struct Test {
	severity: i32
	
}
pub impl Test {
	pub fn main() -> () {
		let mut a: Option<i32> = None, b: f32 = f32::NaN, c: i8 = 0i8, d: Option<@Test> = None;
		a = None;
		if (a.is_none()) {
			io::println(@"A IS AN NULL");
			}
		d = Test::new();
		io::println(Some(d.unwrap().severity.to_str().to_owned()).unwrap());
		}
	
	pub fn new() -> Option<@Test> {
		let mut self = Test {severity: 0i32};
		self.severity = 215692i32;
		return Some(@self);
		}
	}
impl lib::HxObject for Test {
	}
impl ToStr for Test {
	pub fn to_str(&self) -> ~str {
		return ~"Test";
	}
}
