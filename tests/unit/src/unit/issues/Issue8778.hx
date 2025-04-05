package unit.issues;

class Issue8778 extends unit.Test {
	function test() {
		eq("haxe.macro.Position", runMacro("foo"));
	}

	macro static function runMacro(e:haxe.macro.Expr) {
		var c = Type.getClass(e.pos);
		var fields = Type.getInstanceFields(c);
		return macro $v{Type.getClassName(c)};
	}
}