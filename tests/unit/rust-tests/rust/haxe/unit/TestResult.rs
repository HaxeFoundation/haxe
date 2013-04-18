mod HxObject;
mod StringBuf;
use rust::Lib;
mod Std;
use haxe::unit::TestStatus;
mod List;
pub struct TestResult {
	success: bool, 
	m_tests: Option<@List<Option<@haxe::unit::TestStatus>><Option<@haxe::unit::TestStatus>>>
}
pub impl TestResult {
	pub fn toString(&mut self) -> Option<@str> {
		let mut buf: Option<@StringBuf> = StringBuf::new();
		let mut failures: i32 = 0i32;
		let mut _it = (rust::Lib::unwrap((rust::Lib::unwrap(self)).m_tests)).iterator();
		while _it.hasNext() {
		let test:Option<@haxe::unit::TestStatus> = _it.next();
		{
			if ((rust::Lib::unwrap(test)).success == false) {
				{
					(rust::Lib::unwrap(buf)).b += Some(@"* ");
					(rust::Lib::unwrap(buf)).b
				}
				{
					(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap(test)).classname);
					(rust::Lib::unwrap(buf)).b
				}
				{
					(rust::Lib::unwrap(buf)).b += Some(@"::");
					(rust::Lib::unwrap(buf)).b
				}
				{
					(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap(test)).method);
					(rust::Lib::unwrap(buf)).b
				}
				{
					(rust::Lib::unwrap(buf)).b += Some(@"()");
					(rust::Lib::unwrap(buf)).b
				}
				{
					(rust::Lib::unwrap(buf)).b += Some(@"\n");
					(rust::Lib::unwrap(buf)).b
				}
				{
					(rust::Lib::unwrap(buf)).b += Some(@"ERR: ");
					(rust::Lib::unwrap(buf)).b
				}
				if ((rust::Lib::unwrap(test)).posInfos != None) {
					{
						(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap((rust::Lib::unwrap(test)).posInfos)).fileName);
						(rust::Lib::unwrap(buf)).b
					}
					{
						(rust::Lib::unwrap(buf)).b += Some(@":");
						(rust::Lib::unwrap(buf)).b
					}
					{
						(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap((rust::Lib::unwrap(test)).posInfos)).lineNumber);
						(rust::Lib::unwrap(buf)).b
					}
					{
						(rust::Lib::unwrap(buf)).b += Some(@"(");
						(rust::Lib::unwrap(buf)).b
					}
					{
						(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap((rust::Lib::unwrap(test)).posInfos)).className);
						(rust::Lib::unwrap(buf)).b
					}
					{
						(rust::Lib::unwrap(buf)).b += Some(@".");
						(rust::Lib::unwrap(buf)).b
					}
					{
						(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap((rust::Lib::unwrap(test)).posInfos)).methodName);
						(rust::Lib::unwrap(buf)).b
					}
					{
						(rust::Lib::unwrap(buf)).b += Some(@") - ");
						(rust::Lib::unwrap(buf)).b
					}
				}
				{
					(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap(test)).error);
					(rust::Lib::unwrap(buf)).b
				}
				{
					(rust::Lib::unwrap(buf)).b += Some(@"\n");
					(rust::Lib::unwrap(buf)).b
				}
				if ((rust::Lib::unwrap(test)).backtrace != None) {
					{
						(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap(test)).backtrace);
						(rust::Lib::unwrap(buf)).b
					}
					{
						(rust::Lib::unwrap(buf)).b += Some(@"\n");
						(rust::Lib::unwrap(buf)).b
					}
				}
				{
					(rust::Lib::unwrap(buf)).b += Some(@"\n");
					(rust::Lib::unwrap(buf)).b
				}
				{
					failures += 1;
					failures - 1
				};
				}
		}
		}
		{
			(rust::Lib::unwrap(buf)).b += Some(@"\n");
			(rust::Lib::unwrap(buf)).b
		}
		if (failures == 0i32) {
			{
				(rust::Lib::unwrap(buf)).b += Some(@"OK ");
				(rust::Lib::unwrap(buf)).b
			}
		}
		else {
			{
				(rust::Lib::unwrap(buf)).b += Some(@"FAILED ");
				(rust::Lib::unwrap(buf)).b
			}
		}
		{
			(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap((rust::Lib::unwrap(self)).m_tests)).length);
			(rust::Lib::unwrap(buf)).b
		}
		{
			(rust::Lib::unwrap(buf)).b += Some(@" tests, ");
			(rust::Lib::unwrap(buf)).b
		}
		{
			(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string(failures);
			(rust::Lib::unwrap(buf)).b
		}
		{
			(rust::Lib::unwrap(buf)).b += Some(@" failed, ");
			(rust::Lib::unwrap(buf)).b
		}
		{
			(rust::Lib::unwrap(buf)).b += (rust::Lib::unwrap(Std))::string((rust::Lib::unwrap((rust::Lib::unwrap(self)).m_tests)).length - failures);
			(rust::Lib::unwrap(buf)).b
		}
		{
			(rust::Lib::unwrap(buf)).b += Some(@" success");
			(rust::Lib::unwrap(buf)).b
		}
		{
			(rust::Lib::unwrap(buf)).b += Some(@"\n");
			(rust::Lib::unwrap(buf)).b
		}
		return (rust::Lib::unwrap(buf)).b;
	}
	pub fn add(&mut self, t: Option<@haxe::unit::TestStatus>) -> () {
		(rust::Lib::unwrap((rust::Lib::unwrap(self)).m_tests)).add(t);
		if (!(rust::Lib::unwrap(t)).success) {
			{
				(rust::Lib::unwrap(self)).success = false;
				(rust::Lib::unwrap(self)).success
			}
		}
	}
	pub fn new() -> Option<@haxe::unit::TestResult> {
		let mut self = TestResult {success: false, m_tests: None}
		{
			(rust::Lib::unwrap(self)).m_tests = List::new();
			(rust::Lib::unwrap(self)).m_tests
		}
		{
			(rust::Lib::unwrap(self)).success = true;
			(rust::Lib::unwrap(self)).success
		}
		return Some(@self);
	}
}
impl HxObject for haxe::unit::TestResult {
}
