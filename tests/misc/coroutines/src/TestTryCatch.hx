import hxcoro.Coro.*;
import Helper;

class TestTryCatch extends utest.Test {
	function testTryCatch() {
		Assert.same(["e1", "e2"], CoroRun.run(@:coroutine function run() {
			return mapCalls([new E1(), new E2()], tryCatch);
		}));
	}

	function testTryCatchFail() {
		Assert.raises(() -> CoroRun.run(@:coroutine function run() {
			return tryCatch(new E3());
		}), E3);
	}

	function testTryCatchNonExc() {
		Assert.same(["ne1", "ne2"], CoroRun.run(@:coroutine function run() {
			return mapCalls([new NE1(), new NE2()], tryCatchNonExc);
		}));
	}

	function testTryCatchNonExcFail() {
		Assert.raises(() -> CoroRun.run(@:coroutine function run() {
			return tryCatchNonExc(new NE3());
		}), NE3);
	}

	function testTryCatchMixed() {
		Assert.same(["e1", "e2", "ne1", "ne2"], CoroRun.run(@:coroutine function run() {
			return mapCalls(([new E1(), new E2(), new NE1(), new NE2()] : Array<Dynamic>), tryCatchMixed);
		}));
	}

	function testTryCatchMixedFail() {
		Assert.raises(() -> CoroRun.run(@:coroutine function run() {
			return tryCatchMixed("foo");
		}), String);
		Assert.raises(() -> CoroRun.run(@:coroutine function run() {
			return tryCatchMixed(new E3());
		}), E3);
		Assert.raises(() -> CoroRun.run(@:coroutine function run() {
			return tryCatchMixed(new NE3());
		}), NE3);
	}

	function testTryCatchNoCatch() {
		@:coroutine function f(yield:Coroutine<Int->Void>) {
			var dummy = '1';
			try {
				dummy += '2';
				yield(10);
				dummy += '3';
			} catch (e:Dynamic) {
				dummy += '4';
			}
			dummy += '5';
			return dummy;
		}
		var a = [];
		Assert.equals("1235", CoroRun.run(() -> f(i -> a.push(i))));
		Assert.same([10], a);
		a = [];
		Assert.equals("1245", CoroRun.run(() -> f(i -> throw i)));
		Assert.same([], a);
	}

	function testTryCatchOneCatch() {
		@:coroutine function f(yield:Coroutine<Int->Void>) {
			var dummy = '1';
			try {
				dummy += '2';
				throw 'Error!';
				dummy += '3';
			} catch (e:Dynamic) {
				dummy += '4';
				yield(10);
				dummy += '5';
			}
			dummy += '6';
			return dummy;
		}
		var a = [];
		Assert.equals("12456", CoroRun.run(() -> f(i -> a.push(i))));
		Assert.same([10], a);
	}

	function testTryCatchMultiCatch() {
		@:coroutine function f(yield:Coroutine<Int->Void>, throwValue:Dynamic) {
			var dummy = '1';
			try {
				dummy += '2';
				throw throwValue;
				dummy += '3';
			} catch (e:String) {
				dummy += '4';
				yield(10);
				dummy += '5';
			} catch (e:Dynamic) {
				dummy += '6';
				yield(20);
				dummy += '7';
			}
			dummy += '8';
			return dummy;
		}
		var a = [];
		Assert.equals("12458", CoroRun.run(() -> f(i -> a.push(i), 'Error')));
		Assert.same([10], a);
		a = [];
		Assert.equals("12678", CoroRun.run(() -> f(i -> a.push(i), 123)));
		Assert.same([20], a);
	}

	function testTryCatchNested() {
		@:coroutine function f(yield:Coroutine<String->Void>, throwValue:Dynamic) {
			var dummy = '1';
			try {
				try {
					dummy += '2';
					throw throwValue;
					dummy += '3';
				} catch (e:Int) {
					dummy += '4';
					yield("10");
					dummy += '5';
				}
				dummy += '6';
			} catch (e:Dynamic) {
				dummy += '7';
				yield('caught: $e, dummy: $dummy');
				dummy += '8';
			}
			dummy += '9';
			return dummy;
		}
		var a = [];
		Assert.equals("124569", CoroRun.run(() -> f(i -> a.push(i), 1)));
		Assert.same(["10"], a);
		a = [];
		Assert.equals("12789", CoroRun.run(() -> f(i -> a.push(i), "foo")));
		Assert.same(["caught: foo, dummy: 127"], a);
		a = [];
		Assert.equals("124789", CoroRun.run(() -> f(i -> i == "10"?throw i:a.push(i), 1)));
		Assert.same(["caught: 10, dummy: 1247"], a);
		final yieldThrow = @:coroutine i -> throw i;
		// TODO: gives "Cannot use Void as value" without the explicit :Void type-hint
		final yieldThrowInChildCoro = @:coroutine function(i):Void return CoroRun.run(() -> throw i);
		for (yield in [yieldThrow, yieldThrowInChildCoro]) {
			try {
				CoroRun.run(() -> f(yield, "foo"));
				Assert.fail();
			} catch (e:String) {
				Assert.equals('caught: foo, dummy: 127', e);
			}
			try {
				CoroRun.run(() -> f(yield, 1));
				Assert.fail();
			} catch (e:String) {
				Assert.equals('caught: 10, dummy: 1247', e);
			}
		}
	}

	function testTryCatchExceptionNotCaughtThrownOutOfYieldContext() { // wtf?
		var dummy = '1';
		@:coroutine function f(yield:Coroutine<Int->Void>) {
			try {
				dummy += '2';
				throw "Error!";
				dummy += '3';
				yield(10);
				dummy += '4';
			} catch (e:Int) {
				dummy += '5';
			}
			dummy += '6';
			return dummy;
		}
		try {
			CoroRun.run(() -> f(i -> Assert.fail()));
			Assert.fail();
		} catch (e:String) {
			Assert.equals('Error!', e);
			Assert.equals('12', dummy);
		}
	}

	function testTryCatchYieldCapture() {
		@:coroutine function f(yield:Coroutine<Int->Void>) {
			var dummy = '1';
			try {
				dummy += '2';
				throw 10;
				dummy += '3';
			} catch (e:Int) {
				dummy += '4';
				yield(e);
				dummy += '5';
			}
			dummy += '6';
			return dummy;
		}
		var a = [];
		Assert.equals("12456", CoroRun.run(() -> f(i -> a.push(i))));
		Assert.same([10], a);
	}

	@:coroutine function tryCatch(e:haxe.Exception) {
		try {
			throw e;
		} catch (e:E1) {
			return "e1";
		} catch (e:E2) {
			return "e2";
		}
		return "none";
	}

	@:coroutine function tryCatchNonExc(e:NE) {
		try {
			throw e;
		} catch (e:NE1) {
			return "ne1";
		} catch (e:NE2) {
			return "ne2";
		}
		return "none";
	}

	@:coroutine function tryCatchMixed(e:Any) {
		try {
			throw e;
		} catch (e:E1) {
			return "e1";
		} catch (e:E2) {
			return "e2";
		} catch (e:NE1) {
			return "ne1";
		} catch (e:NE2) {
			return "ne2";
		}
		return "none";
	}
}

private class E1 extends haxe.Exception {
	public function new()
		super("E1");
}

private class E2 extends haxe.Exception {
	public function new()
		super("E2");
}

private class E3 extends haxe.Exception {
	public function new()
		super("E3");
}

interface NE {}

private class NE1 implements NE {
	public function new() {};
}

private class NE2 implements NE {
	public function new() {};
}

private class NE3 implements NE {
	public function new() {};
}
