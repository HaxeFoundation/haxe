mod lib;
pub struct Test;
pub impl Test {
	pub fn main() -> () {
		io::println(@"Hello, world!");
		}
	}
impl lib::HxObject for Test {
	}
impl ToStr for Test {
	pub fn to_str(&self) -> ~str {
		return ~"Test";
	}}
