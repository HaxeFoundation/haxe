package unit.issues;

#if macro
class Issue9812 {
	static macro function store(e) {
		return haxe.macro.Context.storeTypedExpr(haxe.macro.Context.typeExpr(e));
	}
}
#else
class Issue9812 extends unit.Test {
	function test() {
		eq(Foo.Baz, store(Foo.Baz));
	}

	static macro function store(e:haxe.macro.Expr):haxe.macro.Expr;
}

private enum Foo {
	@:native('BAR') Baz;
}
#end