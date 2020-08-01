package unit.issues;

class Issue9667 extends unit.Test {
	var evaluationOrder:Array<String> = [];

	function test() {
		var o = {foo:foo(), "bar-1":bar(), baz:baz()};
		eq(1, o.foo);
		eq(2, Reflect.field(o, "bar-1"));
		eq(3, o.baz);
		aeq(['foo', 'bar', 'baz'], evaluationOrder);
	}

	function foo():Int {
		evaluationOrder.push('foo');
		return 1;
	}

	function bar():Int {
		evaluationOrder.push('bar');
		return 2;
	}

	function baz():Int {
		evaluationOrder.push('baz');
		return 3;
	}
}
