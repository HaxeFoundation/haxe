package unit.issues;

private abstract MacroMap(Map<String, String>) from Map<String, String> {
	@:op(a.b)
	macro static function getField(ethis:haxe.macro.Expr, name:String) {
		return macro(cast $ethis : Map<String, String>).get($v{name});
	}

	@:op(a.b)
	macro static function setField(ethis:haxe.macro.Expr, name:String, value:haxe.macro.Expr) {
		return macro(cast $ethis : Map<String, String>).set($v{name}, $value);
	}
}

class Issue7605 extends unit.Test {
	function test() {
		var m:MacroMap = ["hello" => "world"];
		eq("world", m.hello);
		m.world = "hello";
		eq("hello", m.world);
	}
}
