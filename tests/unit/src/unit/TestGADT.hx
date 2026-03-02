package unit;

enum Constant<T> {
	CString(s:String):Constant<String>;
	CInt(s:String):Constant<Int>;
	CFloat(s:String):Constant<Float>;
}

enum Binop<S, T> {
	OpAdd:Binop<Float, Float>;
	OpEq:Binop<S, Bool>;
}

enum Expr<T> {
	EConst(c:Constant<T>):Expr<T>;
	EBinop<C>
	(op : Binop<C, T>, e1 : Expr<C>, e2 : Expr<C>) : Expr<T>;
}

// Support types for testSwitchLevelTypeParam
enum abstract SwitchKind<K>(String) {
	var SKString:SwitchKind<String>;
	var SKInt:SwitchKind<Int>;
}

private class UntypedBox<T> {
	public var kind:SwitchKind<T>;
	public var value:T;
	public function new() {}
}

class TestGADT extends Test {
	function testBasic() {
		var ti = 1.22;
		var tb = false;

		var e1 = EConst(CFloat("12"));
		var e2 = EConst(CFloat("8"));
		var e3 = EConst(CFloat("12"));

		var eadd = EBinop(OpAdd, e1, e2);
		var s = eval(eadd);
		HelperMacros.typedAs(s, ti);
		eq(s, 20);

		var eeq = EBinop(OpEq, e1, e2);
		var s = eval(eeq);
		HelperMacros.typedAs(s, tb);
		eq(s, false);

		var eeq = EBinop(OpEq, e1, e3);
		var s = eval(eeq);
		HelperMacros.typedAs(s, tb);
		eq(s, true);
	}

	// Test switch-level type parameter refinement for free monomorphisms:
	// When the switch subject contains an enum-abstract with a free mono as
	// type arg (e.g. SwitchKind<Unknown>), each case independently refines
	// that mono — no explicit function-level <T> needed.
	function testSwitchLevelTypeParam() {
		var expectedStr = "";
		var expectedInt = 0;

		// box.kind / box.value have free-mono type (UntypedBox<Unknown<0>>)
		// before the type parameter is bound by any assignment.
		var box = new UntypedBox();
		// The per-case substitution happens entirely at compile time; runtime
		// values happen to be null, but that does not affect the type checks.
		switch [box.kind, box.value] {
			case [SKString, s]:
				// New feature: s is refined to String (not Unknown) because
				// SKString : SwitchKind<String> constrains the free mono.
				HelperMacros.typedAs(s, expectedStr);
			case [SKInt, n]:
				// Similarly n is refined to Int.
				HelperMacros.typedAs(n, expectedInt);
		}
		t(true); // reached without compile error
	}

	@:haxe.warning("-WGenerator")
	static function evalConst<T>(c:Constant<T>):T {
		return switch (c) {
			case CString(s): s;
			case CInt(i): Std.parseInt(i);
			case CFloat(f): Std.parseFloat(f);
		}
	}

	@:haxe.warning("-WGenerator")
	static function evalBinop<T, C>(op:Binop<C, T>, e1:Expr<C>, e2:Expr<C>):T {
		return switch (op) {
			case OpAdd: eval(e1) + eval(e2);
			case OpEq: eval(e1) == eval(e2);
		}
	}

	static function eval<T>(e:Expr<T>):T {
		return switch (e) {
			case EConst(c): evalConst(c);
			case EBinop(_op, _e1, _e2): evalBinop(_op, _e1, _e2); // TODO: this generates some unused variable warnings in macro context (issue #1675?)
		}
	}
}
