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
		new schedulers.TestVirtualTimeScheduler(),
		new structured.TestChildScopes(),
		new structured.TestLazyScopes(),
		new structured.TestThrowingScopes(),
		new structured.TestCoroutineScope()
	];

	var runner = new utest.Runner();

	for (eachCase in cases) {
		runner.addCase(eachCase);
	}
	runner.addCases("issues");

    utest.ui.Report.create(runner);
    runner.run();
}