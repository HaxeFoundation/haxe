package unit.issues;

class IssueLuaJit extends Test {
	function testGotoContinue() {
		#if lua_jit
		var sum = 0;
		for (i in 0...10) {
			if (i % 2 == 0)
				continue;
			sum += i;
		}
		eq(sum, 25); // 1 + 3 + 5 + 7 + 9

		// While loop with continue
		var result = [];
		var j = 0;
		while (j < 5) {
			j++;
			if (j == 3)
				continue;
			result.push(j);
		}
		eq(result.length, 4);
		eq(result[0], 1);
		eq(result[1], 2);
		eq(result[2], 4);
		eq(result[3], 5);
		#else
		noAssert();
		#end
	}

	function testBreakWithContinue() {
		#if lua_jit
		// Verify break still works correctly in loops that also use continue
		var sum = 0;
		for (i in 0...10) {
			if (i >= 5)
				break;
			if (i % 2 == 0)
				continue;
			sum += i;
		}
		eq(sum, 4); // 1 + 3
		#else
		noAssert();
		#end
	}

	function testArrayOperations() {
		#if lua_jit
		var arr = [1, 2, 3, 4, 5];
		eq(arr.length, 5);
		eq(arr[0], 1);
		eq(arr[4], 5);

		arr.push(6);
		eq(arr.length, 6);
		eq(arr[5], 6);

		var empty:Array<Int> = [];
		eq(empty.length, 0);
		empty.push(42);
		eq(empty[0], 42);
		#else
		noAssert();
		#end
	}

	function testGotoContinueInTryCatch() {
		#if lua_jit
		// Continue inside try-catch should use _hx_pcall_continue sentinel
		var sum = 0;
		for (i in 0...10) {
			try {
				if (i % 2 == 0)
					continue;
				sum += i;
			} catch (e:Dynamic) {
				// should not reach here
			}
		}
		eq(sum, 25); // 1 + 3 + 5 + 7 + 9

		// Nested try-catch with continue (re-throw path)
		var result = [];
		for (i in 0...5) {
			try {
				try {
					if (i == 2)
						continue;
				} catch (inner:Dynamic) {
					// should not reach here
				}
				result.push(i);
			} catch (outer:Dynamic) {
				// should not reach here
			}
		}
		eq(result.length, 4);
		eq(result[0], 0);
		eq(result[1], 1);
		eq(result[2], 3);
		eq(result[3], 4);
		#else
		noAssert();
		#end
	}

	function testBreakInTryCatch() {
		#if lua_jit
		// Break inside try-catch should use _hx_pcall_break sentinel
		var sum = 0;
		for (i in 0...10) {
			try {
				if (i >= 5)
					break;
				sum += i;
			} catch (e:Dynamic) {
				// should not reach here
			}
		}
		eq(sum, 10); // 0 + 1 + 2 + 3 + 4

		// Both break and continue in try-catch
		var result = [];
		for (i in 0...10) {
			try {
				if (i % 2 == 0)
					continue;
				if (i >= 7)
					break;
				result.push(i);
			} catch (e:Dynamic) {
				// should not reach here
			}
		}
		// i=0 skip, i=1 push, i=2 skip, i=3 push, i=4 skip, i=5 push, i=6 skip, i=7 break
		eq(result.length, 3);
		eq(result[0], 1);
		eq(result[1], 3);
		eq(result[2], 5);
		#else
		noAssert();
		#end
	}

	function testNestedLoopContinue() {
		#if lua_jit
		var results = [];
		for (i in 0...3) {
			for (j in 0...3) {
				if (j == 1)
					continue;
				results.push(i * 10 + j);
			}
		}
		// Should skip j==1 for each i: [0,2, 10,12, 20,22]
		eq(results.length, 6);
		eq(results[0], 0);
		eq(results[1], 2);
		eq(results[2], 10);
		eq(results[3], 12);
		eq(results[4], 20);
		eq(results[5], 22);
		#else
		noAssert();
		#end
	}
}
