package unit;

import haxe.Rest;

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
			var a = r.toArray();
			a[0] = 999; //make sure array modification doesn't affect rest arguments object
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

	@:depends(testToArray, testRestReturn)
	function testInheritance() {
		var p = new Parent(1, 2, 3);
		var c = new Child(4, 5, 6);
		aeq([1, 2, 3], p.ctorArgs.toArray());
		aeq([4, 5, 6, 999], c.ctorArgs.toArray());

		aeq([7, 8, 9], p.methodWithRest(7, 8, 9).toArray());
		aeq([7, 8, 9, 999], c.methodWithRest(7, 8, 9).toArray());
	}

	function testInline() {
		inline function rest(args:Rest<Int>) {
			return args[2];
		}
		eq(3, rest(1, 2, 3, 4));
	}
}

private class Parent {
	public final ctorArgs:Rest<Int>;

	public function new(rest:Rest<Int>) {
		ctorArgs = rest;
	}

	public function methodWithRest(rest:Rest<Int>):Rest<Int> {
		return rest;
	}
}

private class Child extends Parent {
	public function new(rest:Rest<Int>) {
		super(...rest.append(999));
	}

	override public function methodWithRest(rest:Rest<Int>):Rest<Int> {
		return super.methodWithRest(...rest.append(999));
	}
}