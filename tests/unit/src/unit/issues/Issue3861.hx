package unit.issues;

class Issue3861 extends unit.Test {
	@:haxe.warning("-WVarInit")
	function test() {
		var a;
		var b = function() return a;
		a = 2;
		eq(2, b());
	}
}
