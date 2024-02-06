class TestControlFlow extends utest.Test {
	function testIfThen(async:Async) {
		@:coroutine function f(x) {
			if (x) return 1;
			return 2;
		}
		mapCalls.start([true, false], f, (result,error) -> {
			Assert.same([1, 2], result);
			async.done();
		});
	}

	function testIfThenReturnNoValue(async:Async) {
		var v;
		@:coroutine function f(x) {
			v = 1;
			if (x) {
				return;
			}
			v = 2;
		}
		@:coroutine function f2(x) { f(x); return v; }
		mapCalls.start([true, false], f2, (result,error) -> {
			Assert.same([1, 2], result);
			async.done();
		});
	}

	function testIfThenElse(async:Async) {
		@:coroutine function f(x) {
			return if (x) 1 else 2;
		}
		mapCalls.start([true, false], f, (result,error) -> {
			Assert.same([1, 2], result);
			async.done();
		});
	}

	function testSwitchNoDefault(async:Async) {
		@:coroutine function f(x) {
			switch (x) {
				case 1: return "a";
				case 2: return "b";
				case 3: return "c";
			}
			return "d";
		}
		mapCalls.start([1, 2, 3, 4], f, (result,error) -> {
			Assert.same(["a", "b", "c", "d"], result);
			async.done();
		});
	}

	function testSwitchDefault(async:Async) {
		@:coroutine function f(x) {
			switch (x) {
				case 1: return "a";
				case 2: return "b";
				case 3: return "c";
				default: return "d";
			}
			return "e";
		}
		mapCalls.start([1, 2, 3, 4], f, (result,error) -> {
			Assert.same(["a", "b", "c", "d"], result);
			async.done();
		});
	}

	function testLoop(async:Async) {
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
		mapCalls.start([0, 1, 2], f, (result,error) -> {
			Assert.same([
				[0,1,2,3,4,5,6,7,8,9],
				[0,1,2,3,4],
				[0,1,2,3,4,5,7,8,9]
			], result);
			async.done();
		});
	}

	function testTryCatch(async:Async) {
		mapCalls.start([new E1(), new E2()], tryCatch, (result,error) -> {
			Assert.same(["e1", "e2"], result);
			async.done();
		});
	}

	function testTryCatchFail(async:Async) {
		tryCatch.start(new E3(), (result,error) -> {
			Assert.isOfType(error, E3);
			async.done();
		});
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
}

@:coroutine
private function mapCalls<TArg,TRet>(args:Array<TArg>, f:Coroutine<TArg->TRet>):Array<TRet> {
	return [for (arg in args) f(arg)];
}

private class E1 extends haxe.Exception {
	public function new() super("E1");
}

private class E2 extends haxe.Exception {
	public function new() super("E1");
}
private class E3 extends haxe.Exception {
	public function new() super("E1");
}
