mod lib;
pub struct Test {
	value: Option<~str>
	
}
pub impl Test {
	pub fn main() -> () {
		io::println(Test::new(Some(~"Hallo, wurld.")).unwrap().value.unwrap());
		}
	
	pub fn new(val: Option<~str>) -> Option<~Test> {
		let mut self = ~Test {value: None};
		self.value = val;
		return Some(self);
		}
	}
impl lib::HxObject for Test {
	}
impl ToStr for Test {
	pub fn to_str(&self) -> ~str {
		return ~"Test";
	}}
