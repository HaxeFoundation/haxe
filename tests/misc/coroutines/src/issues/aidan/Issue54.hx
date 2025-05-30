package issues.aidan;

@:coroutine function suspendThenThrow() {
	delay(1);
	throw "fail";
}

@:coroutine function f() {
	try {
		suspendThenThrow();
		return "wrong";
	} catch (e:Dynamic) {
		return 'caught: $e';
	}
}

class Issue54 extends utest.Test {
	public function test() {
		Assert.equals("caught: fail", CoroRun.run(f));
	}
}