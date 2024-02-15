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
	function testCapturedThis(async:Async) {
		var file = new CoroFile("value");
		file.write.start((result, _) -> {
			Assert.equals("value", result);
			async.done();
		});
	}

	function testPreviouslyCapturedThis(async:Async) {
		var file = new CoroFile("value");
		file.almostWrite.start((result, _) -> {
			Assert.equals("value", result());
			async.done();
		});
	}
}