package unit.issues;

private extern class Ext {
	static macro inline function c():haxe.macro.Expr {
		return macro "c";
	}
}

class Issue9209 extends unit.Test {
	function test() {
		// macro functions of extern classes are public by default, like other extern fields
		eq("c", Ext.c());
	}
}
