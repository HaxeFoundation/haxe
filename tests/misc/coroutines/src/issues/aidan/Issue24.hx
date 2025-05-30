package issues.aidan;

class MyCont {
	public function new() {}

	public function getOrThrow():Any {
		return "foo";
	}
}

@:coroutine
private function await() {
	var safe = new MyCont();
	return {
		var this1 = safe.getOrThrow();
		this1;
	};
}

class Issue24 extends utest.Test {
	function test() {
		Assert.equals("foo", CoroRun.run(await));
	}
}