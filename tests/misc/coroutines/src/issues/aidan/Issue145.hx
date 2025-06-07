package issues.aidan;

@:coroutine function throwing() {
	throw "throwing";
}

@:coroutine function throwingAfterYield() {
	yield();
	throw "throwing";
}

class Issue145 extends utest.Test {
	function testSurprisinglySimple1() {
		final result = CoroRun.run(() -> try {
			throwing();
			"oh no";
		} catch(s:String) {
			s;
		});
		Assert.equals("throwing", result);

	}
	function testSurprisinglySimple2() {
		final result = CoroRun.run(() -> try {
			throwingAfterYield();
			"oh no";
		} catch (s:String) {
			s;
		});
		Assert.equals("throwing", result);
	}
}