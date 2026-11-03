package unit;

private enum Constant<T> {
	CString(s:String):Constant<String>;
	CInt(s:String):Constant<Int>;
	CFloat(s:String):Constant<Float>;
}

private enum Binop<S, T> {
	OpAdd:Binop<Float, Float>;
	OpEq:Binop<S, Bool>;
}

private enum Expr<T> {
	EConst(c:Constant<T>):Expr<T>;
	EBinop<C>(op:Binop<C, T>, e1:Expr<C>, e2:Expr<C>):Expr<T>;
}

// Support types for testSwitchLevelTypeParam
private enum abstract SwitchKind<K>(String) {
	var SKString:SwitchKind<String>;
	var SKInt:SwitchKind<Int>;
}

private class UntypedBox<T> {
	public var kind:SwitchKind<T>;
	public var value:T;

	public function new() {}
}

// Support types for testSwitchLevelTypeParamEnum (TEnum variant of the same test)
private enum EnumTag<T> {
	ETString:EnumTag<String>;
	ETInt:EnumTag<Int>;
}

private class UntypedBox2<T> {
	public var tag:EnumTag<T>;
	public var value:T;

	public function new() {}
}

private enum abstract MyTypeKind<K>(String) {
	var TInst:MyTypeKind<String>;
	var TEnum:MyTypeKind<Int>;
}

private typedef MyType<T> = {
	var kind:MyTypeKind<T>;
	var args:T;
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

	function testFieldMatches() {
		function matchTypeAsTuple<T>(t:Null<MyType<T>>):T {
			return switch [t.kind, t.args] {
				case [TInst, name]:
					HelperMacros.typedAs(name, "");
					name;
				case [TEnum, id]:
					HelperMacros.typedAs(id, 0);
					id;
			}
		}

		function matchTypeField<T>(t:Null<MyType<T>>):T {
			return switch (t.kind) {
				case TInst:
					HelperMacros.typedAs(t.args, "");
					t.args;
				case TEnum:
					HelperMacros.typedAs(t.args, 0);
					t.args;
			}
		}

		function matchTypeDirect<T>(t:Null<MyType<T>>):T {
			return switch (t) {
				case {kind: TInst}:
					HelperMacros.typedAs(t.args, "");
					t.args;
				case {kind: TEnum}:
					HelperMacros.typedAs(t.args, 0);
					t.args;
			}
		}

		final tInst = {kind: TInst, args: "MyClass"};
		final tEnum = {kind: TEnum, args: 12};
		eq("MyClass", matchTypeAsTuple(tInst));
		eq(12, matchTypeAsTuple(tEnum));
		eq("MyClass", matchTypeField(tInst));
		eq(12, matchTypeField(tEnum));
		eq("MyClass", matchTypeDirect(tInst));
		eq(12, matchTypeDirect(tEnum));
	}

	#if todo
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
		// This test is purely a compile-time type-refinement check:
		// HelperMacros.typedAs asserts at compile time that the types match.
		// Runtime values are null (UntypedBox fields are uninitialized), which
		// is fine since the feature being tested is the per-case type narrowing.
		switch [box.kind, box.value] {
			case [SKString, s]:
				// New feature: s is refined to String (not Unknown) because
				// SKString : SwitchKind<String> constrains the free mono.
				HelperMacros.typedAs(s, expectedStr);
			case [SKInt, n]:
				// Similarly n is refined to Int.
				HelperMacros.typedAs(n, expectedInt);
		}
		t(true); // compile-time type checks passed
	}

	// Test switch-level type parameter refinement for free monomorphisms in TEnum:
	// Same as testSwitchLevelTypeParam but using a plain enum instead of enum abstract.
	function testSwitchLevelTypeParamEnum() {
		var expectedStr = "";
		var expectedInt = 0;

		var box2 = new UntypedBox2();
		// The HelperMacros.typedAs calls below are compile-time type checks;
		// at runtime box2.tag is null so we guard with try/catch.
		try {
			switch [box2.tag, box2.value] {
				case [ETString, s]:
					HelperMacros.typedAs(s, expectedStr);
				case [ETInt, n]:
					HelperMacros.typedAs(n, expectedInt);
			}
		} catch (_:Dynamic) {}
		t(true); // compile-time type checks passed
	}
	#end

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
