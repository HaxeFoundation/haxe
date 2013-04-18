use rust::Lib;
mod HxObject;
use haxe::unit::TestRunner;
pub struct Test;
pub impl Test {
	pub fn testTuple(&mut self) -> () {
		let mut t: Option<(f32, Option<@str>, bool)> = (371.235f32, Some(@"Nopenup"), false);
		(rust::Lib::unwrap(t)).a;
		(rust::Lib::unwrap(t)).b;
		(rust::Lib::unwrap(t)).c;
	}
	pub fn main() -> () {
		let mut r: Option<@haxe::unit::TestRunner> = haxe::unit::TestRunner::new();
		(rust::Lib::unwrap(r)).add(Test::new());
		(rust::Lib::unwrap(r)).run();
	}
	pub fn new() -> Option<@Test> {
		let mut self = Test {}
		self.super()();
		return Some(@self);
	}
}
impl HxObject for Test {
}
