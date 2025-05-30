import Helper;

import hxcoro.Coro.*;

class TestControlFlow extends utest.Test {
	function testIfThen() {
		@:coroutine function f(x) {
			if (x) return 1;
			return 2;
		}

		Assert.same(CoroRun.run(@:coroutine function run() {
			return mapCalls([ true, false ], f);
		}), [ 1, 2 ]);
	}

	function testIfThenReturnNoValue() {
		var v = null;
		@:coroutine function f(x) {
			v = 1;
			if (x) {
				return;
			}
			v = 2;
		}
		@:coroutine function f2(x) { f(x); return v; }

		Assert.same(CoroRun.run(@:coroutine function run() {
			return mapCalls([ true, false ], f2);
		}), [ 1, 2 ]);
	}

	function testIfThenElse() {
		@:coroutine function f(x) {
			return if (x) 1 else 2;
		}

		Assert.same(CoroRun.run(@:coroutine function run() {
			return mapCalls([ true, false ], f);
		}), [ 1, 2 ]);
	}

	function testSwitchNoDefault() {
		@:coroutine function f(x) {
			switch (x) {
				case 1: return "a";
				case 2: return "b";
				case 3: return "c";
			}
			return "d";
		}

		Assert.same(CoroRun.run(@:coroutine function run() {
			return mapCalls([ 1, 2, 3, 4 ], f);
		}), ["a", "b", "c", "d"]);
	}

	function testSwitchDefault() {
		@:coroutine function f(x) {
			switch (x) {
				case 1: return "a";
				case 2: return "b";
				case 3: return "c";
				default: return "d";
			}
			return "e";
		}
		Assert.same(CoroRun.run(@:coroutine function run() {
			return mapCalls([ 1, 2, 3, 4 ], f);
		}), ["a", "b", "c", "d"]);
	}

	function testLoop() {
		@:coroutine function f(x) {
			var results = [];
			var i = 0;
			while (i < 10) {
				if (i == 5 && x == 1) break;
				if (i == 6 && x == 2) { i++; continue; }
				results.push(i);
				i++;
			}
			return results;
		}
		Assert.same([
			[0,1,2,3,4,5,6,7,8,9],
			[0,1,2,3,4],
			[0,1,2,3,4,5,7,8,9]
		], CoroRun.run(@:coroutine function run() {
			return mapCalls([ 0, 1, 2 ], f);
		}));
	}

	function testRecursion() {
		var maxIters = 3;
		var counter = 0;

		@:coroutine function foo() {
			if (++counter < maxIters) {
				foo();
			}
		}

		CoroRun.run(foo);

		Assert.equals(counter, maxIters);
	}

	function testSuspendingRecursion() {
		var maxIters = 3;
		var counter = 0;

		@:coroutine function foo() {
			if (++counter < maxIters) {
				yield();
				foo();
			}
		}

		CoroRun.run(foo);

		Assert.equals(counter, maxIters);
	}
}