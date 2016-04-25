package unit.issues;

import haxe.macro.Expr;

class Issue5162 extends unit.Test {
	function test() {
		var tp = {
			name: "Array",
			pack: [],
			sub: null
		}
		var ct = macro : $tp<Int>;
		t(ct.match(TPath({params: [TPType(TPath({name: "Int"}))]})));

		var tp = {
			name: "Map",
			pack: [],
			sub: null
		}
		var param1 = macro : Int;
		var param2 = macro : String;
		var ct = macro : $tp<$param1, $param2>;
		t(ct.match(TPath({params: [TPType(TPath({name: "Int"})), TPType(TPath({name: "String"}))]})));
	}
}