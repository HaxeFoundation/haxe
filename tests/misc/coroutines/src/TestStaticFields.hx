class TestStaticFields extends utest.Test {
	function testSimpleStart(async:Async) {
		simple.start(42, result -> {
			Assert.equals(42, result);
			async.done();
		});
	}

	function testSimpleCreate(async:Async) {
		var cont = simple.create(42, result -> {
			Assert.equals(42, result);
			async.done();
		});
		cont(null);
	}

	@:coroutine static function simple(arg:Int):Int {
		return arg;
	}
}