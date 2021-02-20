class TestControlFlow extends utest.Test {
	function testIfThen(async:Async) {
		@:coroutine function f(x) {
			if (x) return 1;
			return 2;
		}
		mapCalls.start([true, false], f, result -> {
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
		mapCalls.start([true, false], f2, result -> {
			Assert.same([1, 2], result);
			async.done();
		});
	}

	function testIfThenElse(async:Async) {
		@:coroutine function f(x) {
			return if (x) 1 else 2;
		}
		mapCalls.start([true, false], f, result -> {
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
		mapCalls.start([1, 2, 3, 4], f, result -> {
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
		mapCalls.start([1, 2, 3, 4], f, result -> {
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
		mapCalls.start([0, 1, 2], f, result -> {
			Assert.same([
				[0,1,2,3,4,5,6,7,8,9],
				[0,1,2,3,4],
				[0,1,2,3,4,5,7,8,9]
			], result);
			async.done();
		});
	}
}

@:coroutine
private function mapCalls<TArg,TRet>(args:Array<TArg>, f:Coroutine<TArg->TRet>):Array<TRet> {
	return [for (arg in args) f(arg)];
}
