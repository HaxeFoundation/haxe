extern mod std;
mod lib;
pub struct Test {
	tok: Option<~str>
	, 
	val: i32
	
}
pub impl Test {
	pub fn main() -> () {
		io::println((&Std::string as &lib::HxObject).toString().unwrap());
		io::println(~"Hello, world!");
	}
	pub fn new(t: Option<~str>) -> Option<~Test> {
		let mut self = Test {tok: None, val: 0i32};;
		self.val = 40i32;
		self.tok = t;
		return Some(~self);
	}
}
impl lib::HxObject for Test {
	
	pub fn toString(&self) -> Option<~str> {
		return Some(~"Test: { tok: "+self.tok.toString()+", val: "+self.val.toString()+"}")// Test
	}
}
