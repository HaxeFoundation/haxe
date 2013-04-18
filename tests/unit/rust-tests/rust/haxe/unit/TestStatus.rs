use rust::Lib;
mod HxObject;
pub struct TestStatus {
	backtrace: Option<@str>, 
	posInfos: @HxObject, 
	classname: Option<@str>, 
	method: Option<@str>, 
	error: Option<@str>, 
	success: bool, 
	done: bool
}
pub impl TestStatus {
	pub fn new() -> Option<@haxe::unit::TestStatus> {
		let mut self = TestStatus {backtrace: None, posInfos: None, classname: None, method: None, error: None, success: false, done: false}
		{
			(rust::Lib::unwrap(self)).done = false;
			(rust::Lib::unwrap(self)).done
		}
		{
			(rust::Lib::unwrap(self)).success = false;
			(rust::Lib::unwrap(self)).success
		}
		return Some(@self);
	}
}
impl HxObject for haxe::unit::TestStatus {
}
