package unit.issues;

class Issue9037 extends unit.Test {
	function test() {
		var f = function(args:Array<Dynamic>) {};
		var f:Dynamic = Reflect.makeVarArgs(f);
		f();
		noAssert();
	}
}