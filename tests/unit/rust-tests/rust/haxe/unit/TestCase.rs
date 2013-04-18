use rust::Lib;
mod HxObject;
use haxe::unit::TestRunner;
mod Std;
use haxe::unit::TestStatus;
pub struct TestCase {
	currentTest: Option<@haxe::unit::TestStatus>
}
pub impl TestCase {
	pub fn assertEquals(&mut self, expected: Option<@T>, actual: Option<@T>, c: @HxObject) -> () {
		{
			(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).done = true;
			(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).done
		}
		if (actual != expected) {
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).success = false;
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).success
			}
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).error = Some(@"expected '") + (rust::Lib::unwrap(Std))::string(expected) + Some(@"' but was '") + (rust::Lib::unwrap(Std))::string(actual) + Some(@"'");
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).error
			}
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).posInfos = c;
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).posInfos
			}
			fail!((rust::Lib::unwrap(self)).currentTest);
		}
	}
	pub fn assertFalse(&mut self, b: bool, c: @HxObject) -> () {
		{
			(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).done = true;
			(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).done
		}
		if (b == true) {
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).success = false;
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).success
			}
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).error = Some(@"expected false but was true");
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).error
			}
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).posInfos = c;
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).posInfos
			}
			fail!((rust::Lib::unwrap(self)).currentTest);
		}
	}
	pub fn assertTrue(&mut self, b: bool, c: @HxObject) -> () {
		{
			(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).done = true;
			(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).done
		}
		if (b == false) {
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).success = false;
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).success
			}
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).error = Some(@"expected true but was false");
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).error
			}
			{
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).posInfos = c;
				(rust::Lib::unwrap((rust::Lib::unwrap(self)).currentTest)).posInfos
			}
			fail!((rust::Lib::unwrap(self)).currentTest);
		}
	}
	pub fn print(&mut self, v: @HxObject) -> () {
		(rust::Lib::unwrap(haxe::unit::TestRunner))::print(v);
	}
	pub fn tearDown(&mut self) -> () {
	}
	pub fn setup(&mut self) -> () {
	}
	pub fn new() -> Option<@haxe::unit::TestCase> {
		let mut self = TestCase {currentTest: None}
		return Some(@self);
	}
}
impl HxObject for haxe::unit::TestCase {
}
