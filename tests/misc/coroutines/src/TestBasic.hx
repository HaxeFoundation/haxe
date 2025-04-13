import haxe.Exception;
import haxe.coro.Coroutine.yield;

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

	function testUnnamedLocalCoroutines() {
		final c1 = @:coroutine function () {
			yield();

			return 10;
		};

		Assert.equals(10, Coroutine.run(c1));
	}

	@:coroutine static function simple(arg:Int):Int {
		return arg;
	}

	@:coroutine static function error() {
		throw "nope";
	}
}
