package unit;

import haxe.Rest;

// TODO: add tests for constructors and overridden methods
class TestRest extends Test {
	function testArrayAccess() {
		function rest(a:Int, b:Int, r:Rest<Int>) {
			return r[2];
		}
		eq(123, rest(1, 2, 0, 0, 123, 0));
	}

	function testLength() {
		function rest(r:Rest<Int>) {
			return r.length;
		}
		eq(4, rest(1, 2, 3, 4));
	}

	function testToArray() {
		function rest(r:Rest<Int>):Array<Int> {
			return r.toArray();
		}
		aeq([1, 2, 3, 4], rest(1, 2, 3, 4));
	}

	@:depends(testToArray)
	function testRestReturn() {
		function rest(r:Rest<Int>):Rest<Int> {
			return r;
		}
		aeq([1, 2, 3, 4], rest(1, 2, 3, 4).toArray());
	}

	function testIterator() {
		function rest(r:Rest<Int>):Array<Int> {
			return [for(i in r) i];
		}
		aeq([3, 2, 1], rest(3, 2, 1));
	}

	function testKeyValueIterator() {
		function rest(r:Rest<Int>):{keys:Array<Int>, values:Array<Int>} {
			var keys = [];
			var values = [];
			for(k => v in r) {
				keys.push(k);
				values.push(v);
			}
			return {keys:keys, values:values}
		}
		var r = rest(3, 2, 1, 0);
		aeq([0, 1, 2, 3], r.keys);
		aeq([3, 2, 1, 0], r.values);
	}

	@:depends(testToArray)
	function testAppend() {
		function rest(r:Rest<Int>) {
			var appended = r.append(9);
			return {initial:r.toArray(), appended:appended.toArray()}
		}
		var result = rest(1, 2);
		aeq([1, 2], result.initial);
		aeq([1, 2, 9], result.appended);
	}

	@:depends(testToArray)
	function testSpread() {
		function rest(r:Rest<Int>) {
			return r.toArray();
		}
		function spreadRest(r:Rest<Int>) {
			return rest(...r);
		}
		aeq([1, 2, 3], rest(...[1, 2, 3]));
		aeq([3, 2, 1], spreadRest(3, 2, 1));
	}
}