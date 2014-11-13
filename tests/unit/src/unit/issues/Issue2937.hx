package unit.issues;


class Issue2937 extends Test {
	function test() {
		var f = Reflect.makeVarArgs(function(args:Array<Dynamic>) {
			t(Std.is(args, Array)); // false, but should be true
		});
		f(1,2,3);
	}
}