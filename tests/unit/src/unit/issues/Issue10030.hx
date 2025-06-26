package unit.issues;

class Issue10030 extends unit.Test {
	function test() {
		aeq(([1, true, 'hello']:Array<Dynamic>), restDynamic(1, true, 'hello'));
		aeq(([2, false, 'world']:Array<Dynamic>), restAny(2, false, 'world'));
	}

	function restDynamic(...args:Dynamic) {
		return args.toArray();
	}

	function restAny(...args:Any) {
		return args.toArray();
	}
}