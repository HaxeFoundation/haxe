package unit.issues;
import haxe.macro.Expr;
import unit.Test;

class Issue2634 extends Test {

	function test() {
		var s = mkNew("String", "foo");
		eq(s, "foo");
		var t = mkNew("haxe.Template", "bar");
		eq(t.execute({}), "bar");
	}

	macro static function mkNew(dotPath:String, args:Array<Expr>) {
		var split = dotPath.split(".");
		var name = split.pop();
		var tPath = {
			name: name,
			pack: split,
			params: [],
			sub: null
		}
		var cType = TPath(tPath);
		return macro (new $tPath($a{args}) : $cType);
	}
}