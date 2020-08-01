package unit.issues;

class Issue1284 extends Test {
	function test() {
		m1('test$id', ['test$id'], {'test$id': 'test$id'});
		m2('test$id', ['test$id'], {'test$id': 'test$id'});
	}

	static function check(b, p) if (!b) throw new haxe.macro.Expr.Error("assertion failed", p);

	macro static function m1(a, b, c) {
		check(a.expr.match(EConst(CString("test$id", SingleQuotes))), a.pos);
		check(b.expr.match(EArrayDecl([{expr: EConst(CString("test$id", SingleQuotes))}])), b.pos);
		check(c.expr.match(EObjectDecl([{field: "test$id", expr: {expr: EConst(CString("test$id", SingleQuotes))}}])), c.pos);
		return macro utest.Assert.pass();
	}

	macro static function m2(a:String, b:Array<String>, c:{}) {
		check(a == "test$id", (macro _).pos);
		check(b[0] == "test$id", (macro _).pos);
		check(Reflect.field(c, "test$id") == "test$id", (macro _).pos);
		return macro utest.Assert.pass();
	}
}
