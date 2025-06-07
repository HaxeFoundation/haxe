import yield.*;

function main() {

	var cases = [
		new TestBasic(),
		new TestTricky(),
		new TestControlFlow(),
		new TestTryCatch(),
		new TestHoisting(),
		new TestMisc(),
		new TestTexpr(),
		#if !hl
		new TestGenerator(),
		#end
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
	runner.addCases("ds");
	runner.addCases("concurrent");
	runner.addCases("components");
	runner.addCases("structured");
	runner.addCases("features");
	runner.addCases("schedulers");

    utest.ui.Report.create(runner);
    runner.run();
}