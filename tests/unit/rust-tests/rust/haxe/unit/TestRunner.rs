mod HxObject;
mod StringTools;
use haxe::CallStack;
mod Reflect;
use rust::Lib;
use haxe::Log;
mod Std;
mod Type;
use haxe::unit::TestStatus;
use haxe::unit::TestCase;
mod Class;
use haxe::unit::TestResult;
mod List;
pub struct TestRunner {
	cases: Option<@List<Option<@haxe::unit::TestCase>><Option<@haxe::unit::TestCase>>>, 
	result: Option<@haxe::unit::TestResult>
}
pub impl TestRunner {
	priv fn runCase(&mut self, t: Option<@haxe::unit::TestCase>) -> () {
		let mut old: Option<@@fn(@HxObject, @HxObject)->()> = (rust::Lib::unwrap(haxe::Log))::trace;
		{
			(rust::Lib::unwrap(haxe::Log))::trace = (rust::Lib::unwrap(haxe::unit::TestRunner))::customTrace;
			(rust::Lib::unwrap(haxe::Log))::trace
		}
		let mut cl: Class = (rust::Lib::unwrap(Type))::getClass(t);
		let mut fields: Option<~[Option<@str>]> = (rust::Lib::unwrap(Type))::getInstanceFields(cl);
		(rust::Lib::unwrap(haxe::unit::TestRunner))::print(Some(@"Class: ") + (rust::Lib::unwrap(Type))::getClassName(cl) + Some(@" "));
		{
			let mut _g: i32 = 0i32;
			while (_g < (rust::Lib::unwrap(fields)).len()) {
				let mut f: Option<@str> = (rust::Lib::unwrap(fields))[_g];
				{
					_g += 1;
					_g
				};
				let mut fname: Option<@str> = f;
				let mut field: @HxObject = (rust::Lib::unwrap(Reflect))::field(t,f);
				if ((rust::Lib::unwrap(StringTools))::startsWith(fname,Some(@"test")) && (rust::Lib::unwrap(Reflect))::isFunction(field)) {
					{
						(rust::Lib::unwrap(t)).currentTest = haxe::unit::TestStatus::new();
						(rust::Lib::unwrap(t)).currentTest
					}
					{
						(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).classname = (rust::Lib::unwrap(Type))::getClassName(cl);
						(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).classname
					}
					{
						(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).method = fname;
						(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).method
					}
					(rust::Lib::unwrap(t)).setup();
					let $err = do task::try {
						(rust::Lib::unwrap(Reflect))::callMethod(t,field,[]);
						if ((rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).done) {
							{
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).success = true;
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).success
							}
							(rust::Lib::unwrap(haxe::unit::TestRunner))::print(Some(@"."));
						}
						else {
							{
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).success = false;
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).success
							}
							{
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).error = Some(@"(warning) no assert");
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).error
							}
							(rust::Lib::unwrap(haxe::unit::TestRunner))::print(Some(@"W"));
						}
					}
					if (rust::Lib::is($err, Option<@haxe::unit::TestStatus>)) {
						let mut e = $err{
							(rust::Lib::unwrap(haxe::unit::TestRunner))::print(Some(@"F"));
							{
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).backtrace = (rust::Lib::unwrap(haxe::CallStack))::toString((rust::Lib::unwrap(haxe::CallStack))::exceptionStack());
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).backtrace
							}
						}
					}
					elseif (rust::Lib::is($err, @HxObject)) {
						let mut e = $err{
							(rust::Lib::unwrap(haxe::unit::TestRunner))::print(Some(@"E"));
							{
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).error = Some(@"exception thrown : ") + (rust::Lib::unwrap(Std))::string(e);
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).error
							}
							{
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).backtrace = (rust::Lib::unwrap(haxe::CallStack))::toString((rust::Lib::unwrap(haxe::CallStack))::exceptionStack());
								(rust::Lib::unwrap((rust::Lib::unwrap(t)).currentTest)).backtrace
							}
						}
					}
					(rust::Lib::unwrap((rust::Lib::unwrap(self)).result)).add((rust::Lib::unwrap(t)).currentTest);
					(rust::Lib::unwrap(t)).tearDown();
				}
			}
		}
		(rust::Lib::unwrap(haxe::unit::TestRunner))::print(Some(@"\n"));
		{
			(rust::Lib::unwrap(haxe::Log))::trace = old;
			(rust::Lib::unwrap(haxe::Log))::trace
		}
	}
	pub fn run(&mut self) -> bool {
		{
			(rust::Lib::unwrap(self)).result = haxe::unit::TestResult::new();
			(rust::Lib::unwrap(self)).result
		}
		let mut _it = (rust::Lib::unwrap((rust::Lib::unwrap(self)).cases)).iterator();
		while _it.hasNext() {
		let c:Option<@haxe::unit::TestCase> = _it.next();
		{
			(rust::Lib::unwrap(self)).runCase(c);
		}
		}
		(rust::Lib::unwrap(haxe::unit::TestRunner))::print((rust::Lib::unwrap((rust::Lib::unwrap(self)).result)).toString());
		return (rust::Lib::unwrap((rust::Lib::unwrap(self)).result)).success;
	}
	pub fn add(&mut self, c: Option<@haxe::unit::TestCase>) -> () {
		(rust::Lib::unwrap((rust::Lib::unwrap(self)).cases)).add(c);
	}print: Option<@@fn(@HxObject)->()>
	priv fn customTrace(v: @HxObject, p: @HxObject) -> () {
		(rust::Lib::unwrap(haxe::unit::TestRunner))::print((rust::Lib::unwrap(p)).fileName + Some(@":") + (rust::Lib::unwrap(p)).lineNumber + Some(@": ") + (rust::Lib::unwrap(Std))::string(v) + Some(@"\n"));
	}
	pub fn new() -> Option<@haxe::unit::TestRunner> {
		let mut self = TestRunner {cases: None, result: None}
		{
			(rust::Lib::unwrap(self)).result = haxe::unit::TestResult::new();
			(rust::Lib::unwrap(self)).result
		}
		{
			(rust::Lib::unwrap(self)).cases = List::new();
			(rust::Lib::unwrap(self)).cases
		}
		return Some(@self);
	}
}
impl HxObject for haxe::unit::TestRunner {
}
