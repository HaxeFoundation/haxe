package unit.issues;

class Issue5803 extends Test {
	function test() {
		var expr = macro 'a: $a';
		t(expr.expr.match(EFormat([
			{kind: FRaw("a: ")},
			{kind: FIdent("a",_)}
		])));

		var expr = macro 'a: ${a}';
		t(expr.expr.match(EFormat([
			{kind: FRaw("a: ")},
			{kind: FExpr({
				expr: EConst(CIdent("a"))
			})}
		])));
	}
}
