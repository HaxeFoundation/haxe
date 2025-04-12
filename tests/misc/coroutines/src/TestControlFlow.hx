class TestControlFlow extends utest.Test {
	function testIfThen() {
		@:coroutine function f(x) {
			if (x) return 1;
			return 2;
		}

		Assert.same(Coroutine.run(@:coroutine function run() {
			return mapCalls([ true, false ], f);
		}), [ 1, 2 ]);
	}

	// function testIfThenReturnNoValue(async:Async) {
	// 	var v = null;
	// 	@:coroutine function f(x) {
	// 		v = 1;
	// 		if (x) {
	// 			return;
	// 		}
	// 		v = 2;
	// 	}
	// 	@:coroutine function f2(x) { f(x); return v; }

	// 	Assert.same(Coroutine.run(@:coroutine function run() {
	// 		return mapCalls([ true, false ], f2);
	// 	}), [ 1, 2 ]);
	// }

	function testIfThenElse() {
		@:coroutine function f(x) {
			return if (x) 1 else 2;
		}

		Assert.same(Coroutine.run(@:coroutine function run() {
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

		Assert.same(Coroutine.run(@:coroutine function run() {
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
		Assert.same(Coroutine.run(@:coroutine function run() {
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
		], Coroutine.run(@:coroutine function run() {
			return mapCalls([ 0, 1, 2 ], f);
		}));
	}

	// function testTryCatch() {
	// 	Assert.same(["e1", "e2"], Coroutine.run(@:coroutine function run() {
	// 		return mapCalls([ new E1(), new E2() ], tryCatch);
	// 	}));
	// }

	// function testTryCatchFail() {
	// 	Assert.raises(Coroutine.run(@:coroutine function run() {
	// 		return tryCatch(new E3());
	// 	}), E3);
	// }

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
}

@:coroutine
private function mapCalls<TArg,TRet>(args:Array<TArg>, f:Coroutine<TArg->TRet>):Array<TRet> {
	return [for (arg in args) f(arg)];
}

private class E1 extends haxe.Exception {
	public function new() super("E1");
}
private class E2 extends haxe.Exception {
	public function new() super("E2");
}
private class E3 extends haxe.Exception {
	public function new() super("E3");
}
