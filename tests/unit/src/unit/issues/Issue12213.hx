package unit.issues;

import haxe.CallStack;
import haxe.exceptions.NotImplementedException;

class Issue12213 extends Test {
	function test() {
		try {
			throw new haxe.exceptions.NotImplementedException();
		} catch (e:haxe.exceptions.NotImplementedException) {
			var stack = [
				CFunction,
				Module("Foo"),
				FilePos(null, "file", 1, 2),
				Method("Class", "method"),
				LocalFunction(0)
			];
			e.stack = [];
			// test if we can set again because that might be a special case
			e.stack = stack;
			utest.Assert.same(stack, e.stack);
		}
	}
}
