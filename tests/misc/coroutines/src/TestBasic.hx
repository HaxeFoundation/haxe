import haxe.Exception;

class TestBasic extends utest.Test {
	function testSimple() {
		Assert.equals(42, Coroutine.run(@:coroutine function run() {
			return simple(42);
		}));
	}

	function testErrorDirect() {
		Assert.raises(() -> Coroutine.run(error), String);
	}

	function testErrorPropagation() {
		@:coroutine function propagate() {
			error();
		}
		
		Assert.raises(() -> Coroutine.run(propagate), String);
	}

	function testResumeWithError() {
		@:coroutine function foo() {
			Coroutine.suspend(cont -> {
				cont.resume(null, new Exception(""));
			});
		}

		Assert.raises(() -> Coroutine.run(foo), Exception);
	}

	@:coroutine static function simple(arg:Int):Int {
		return arg;
	}

	@:coroutine static function error() {
		throw "nope";
	}
}
