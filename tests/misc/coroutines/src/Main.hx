import yield.*;

function main() {
	utest.UTest.run([
		new TestBasic(),
		new TestTricky(),
		new TestControlFlow(),
		new TestGenerator(),
		#if js
		new TestJsPromise(),
		#end
		new TestYieldBasic(),
		new TestYieldIf(),
		new TestYieldFor(),
		new TestYieldClosure(),
		new TestYieldSwitch(),
		new TestYieldTryCatch(),
		new TestYieldWhile(),
	]);
}