package unit.issues;

import haxe.macro.Context.*;

#if macro
class Issue9828 {
	static macro function foo(e) {
		var t = typeExpr(e);
		return storeTypedExpr(t);
	}
}
#else
class Issue9828 extends unit.Test {
	var x = BadCtor(123);

	function test() {
		t(Type.enumEq(BadCtor(123), foo(BadCtor(123))));
	}

	static macro function foo(e:haxe.macro.Expr):haxe.macro.Expr;
}

private typedef BadTypedef = BadGadt<Int>;

private enum BadGadt<T> {
	BadCtor(v:Int):BadTypedef;
}
#end