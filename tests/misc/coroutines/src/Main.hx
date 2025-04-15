import yield.*;

function main() {

	var cases = [
		new TestBasic(),
		new TestTricky(),
		new TestControlFlow(),
		new TestHoisting(),
		new TestMisc(),
		// new TestGenerator(),
		#if js
		new TestJsPromise(),
		#end
		// new TestYieldBasic(),
		// new TestYieldIf(),
		// new TestYieldFor(),
		// new TestYieldClosure(),
		// new TestYieldSwitch(),
		// new TestYieldTryCatch(),
		// new TestYieldWhile(),
	];

	var runner = new utest.Runner();

	for (eachCase in cases) {
		runner.addCase(eachCase);
	}
	runner.addCases("issues");

    utest.ui.Report.create(runner);
    runner.run();
}