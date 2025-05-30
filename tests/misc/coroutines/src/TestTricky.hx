class CoroFile {
	public final file:String;

	public function new(file) {
		this.file = file;
	}

	@:coroutine public function write() {
		return file;
	}

	@:coroutine public function almostWrite() {
		return () -> file;
	}
}

class TestTricky extends utest.Test {
	function testCapturedThis() {
		final file = new CoroFile("value");
		Assert.equals("value", cast CoroRun.run(file.write));
	}

	function testPreviouslyCapturedThis() {
		final file = new CoroFile("value");
		final func : ()->String = cast CoroRun.run(file.almostWrite);
		Assert.equals("value", func());
	}
}