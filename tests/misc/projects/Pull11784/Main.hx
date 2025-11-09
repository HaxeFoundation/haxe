function main() {
	foo(0);
}

@:generic function foo<T>(val:T):T {
	return bar(val);
}

macro function bar(expr) {
	var typedExpr = haxe.macro.Context.typeExpr(expr);
	return haxe.macro.Context.storeTypedExpr(typedExpr);
}