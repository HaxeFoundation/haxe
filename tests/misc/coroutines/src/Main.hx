import yield.*;

function main() {

	var cases = [
		new TestBasic(),
		new TestTricky(),
		new TestControlFlow(),
		new TestTryCatch(),
		new TestHoisting(),
		new TestMisc(),
		new TestMutex(),
		new TestTexpr(),
		// new TestGenerator(),
		#if js
		new TestJsPromise(),
		#end
		#if (!coroutine.throw && (jvm || cpp || eval))
		new TestCallStack(),
		#end
	];

	var runner = new utest.Runner();

	for (eachCase in cases) {
		runner.addCase(eachCase);
	}
	runner.addCases("issues");

    utest.ui.Report.create(runner);
    runner.run();
}