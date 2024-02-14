class TestBasic extends utest.Test {
	function testSimpleStart(async:Async) {
		simple.start(42, (result,error) -> {
			Assert.equals(42, result);
			async.done();
		});
	}

	function testSimpleCreate(async:Async) {
		var cont = simple.create(42, (result,error) -> {
			Assert.equals(42, result);
			async.done();
		});
		cont(null, null);
	}

	function testErrorDirect(async:Async) {
		error.start((result, error) -> {
			Assert.equals("nope", error);
			async.done();
		});
	}

	function testErrorPropagation(async:Async) {
		@:coroutine function propagate() {
			error();
		}
		propagate.start((result, error) -> {
			Assert.equals("nope", error);
			async.done();
		});
	}

	@:coroutine static function simple(arg:Int):Int {
		return arg;
	}

	@:coroutine static function error() {
		throw "nope";
	}
}
