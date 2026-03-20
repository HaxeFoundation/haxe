package unit.teststd;

class TestType extends unit.Test {
	public function test() {
		// getClass
		eq(Type.getClass("foo"), String);
		eq(Type.getClass(new C()), C);

		//Issue #1485
		eq(Type.getClass([]), Array);
		eq(Type.getClass(Float), null);
		eq(Type.getClass(null), null);
		eq(Type.getClass(Int), null);
		eq(Type.getClass(Bool), null);
		//Type.getClass(haxe.macro.Expr.ExprDef.EBreak) == null;
		eq(Type.getClass( { } ), null);

		// getEnum
		eq(Type.getEnum(haxe.macro.Expr.ExprDef.EBreak), haxe.macro.Expr.ExprDef);
		eq(Type.getEnum(null), null);

		// getSuperClass
		eq(Type.getSuperClass(String), null);
		eq(Type.getSuperClass(ClassWithToString), null);
		eq(Type.getSuperClass(ClassWithToStringChild), ClassWithToString);
		//Type.getSuperClass(null) == null;

		// getClassName
		eq(Type.getClassName(String), "String");
		eq(Type.getClassName(C), "unit.teststd.C");
		//Type.getClassName(null) == null;
		eq(Type.getClassName(Type.getClass([])), "Array");

		// getEnumName
		//Type.getEnumName(null) == null;
		eq(Type.getEnumName(haxe.macro.Expr.ExprDef), "haxe.macro.ExprDef");

		// resolveClass
		eq(Type.resolveClass("String"), String);
		eq(Type.resolveClass("unit.teststd.C"), C);
		//Type.resolveClass("Float") == null;
		//Type.resolveClass(null) == null;
		eq(Type.resolveClass("MyNonExistingClass"), null);

		// resolveEnum
		//Type.resolveEnum(null) == null;
		eq(Type.resolveEnum("haxe.macro.ExprDef"), haxe.macro.Expr.ExprDef);
		eq(Type.resolveEnum("String"), null);

		// createInstance
		eq(Type.createInstance(String, ["foo"]), "foo");
		//Type.createInstance(null, []) == null;
		eq(Type.createInstance(C, []).v, "var");
		//var t = Type.createInstance(ClassWithCtorDefaultValues, []);
		//t.a == 1;
		//t.b == "foo";
		//var t = Type.createInstance(ClassWithCtorDefaultValues, [2]);
		//t.a == 2;
		//t.b == "foo";
		var c = Type.createInstance(ClassWithCtorDefaultValues, [2, "bar"]);
		eq(c.a, 2);
		eq(c.b, "bar");
		var c2 = Type.createInstance(ClassWithCtorDefaultValues2, [2, "bar"]);
		eq(c2.a, 2);
		eq(c2.b, "bar");
		//var t = Type.createInstance(ClassWithCtorDefaultValuesChild, [2, "bar"]);
		//t.a == 2;
		//t.b == "bar";

		// createEmptyInstance
		//Type.createEmptyInstance(String) == "foo";
		//Type.createEmptyInstance(null, []) == null;
		var c = Type.createEmptyInstance(ClassWithCtorDefaultValues);
		eq(c.a, null);
		eq(c.b, null);
		var c = Type.createEmptyInstance(ClassWithCtorDefaultValuesChild);
		eq(c.a, null);
		eq(c.b, null);

		// createEnum
		var e = Type.createEnum(E, "NoArgs");
		eq(e, NoArgs);
		eq(Type.createEnum(E, "NoArgs", []), NoArgs);
		t(Type.enumEq(Type.createEnum(E, "OneArg", [1]), OneArg(1)));
		t(Type.enumEq(Type.createEnum(E, "RecArg", [e]), RecArg(e)));
		t(Type.enumEq(Type.createEnum(E, "MultipleArgs", [1, "foo"]), MultipleArgs(1, "foo")));

		// createEnumIndex
		var e = Type.createEnumIndex(E, 0);
		eq(e, NoArgs);
		eq(Type.createEnumIndex(E, 0, []), NoArgs);
		eq(Type.createEnumIndex(E, 0, null), NoArgs);
		t(Type.enumEq(Type.createEnumIndex(E, 1, [1]), OneArg(1)));
		t(Type.enumEq(Type.createEnumIndex(E, 2, [e]), RecArg(e)));
		t(Type.enumEq(Type.createEnumIndex(E, 3, [1, "foo"]), MultipleArgs(1, "foo")));
		var e = Type.createEnumIndex(EnumFlagTest, 0);
		eq(e, EA);
		eq(Type.createEnumIndex(EnumFlagTest, 1, []), EB);
		eq(Type.createEnumIndex(EnumFlagTest, 2, null), EC);

		// getInstanceFields
		var fields = Type.getInstanceFields(C);
		var requiredFields = ["func", "v", "prop"];
		for (f in fields)
			t(requiredFields.remove(f));
		eq(requiredFields.length, 0);
		var fields = Type.getInstanceFields(CChild);
		var requiredFields = ["func", "v", "prop"];
		for (f in fields)
			t(requiredFields.remove(f));
		eq(requiredFields.length, 0);
		var fields = Type.getClassFields(C);
		var requiredFields = ["staticFunc", "staticVar", "staticProp"];
		for (f in fields)
			t(requiredFields.remove(f));
		eq(requiredFields.length, 0);
		var fields = Type.getClassFields(CChild);
		var requiredFields = [];
		for (f in fields)
			t(requiredFields.remove(f));
		eq(requiredFields.length, 0);

		// getEnumConstructs
		aeq(["NoArgs", "OneArg", "RecArg", "MultipleArgs"], Type.getEnumConstructs(E));
		aeq(["EA", "EB", "EC"], Type.getEnumConstructs(EnumFlagTest));

		// typeof
		eq(Type.typeof(1.5), TFloat);
		eq(Type.typeof(-45), TInt);
		#if (!php && !python)
		eq(Type.typeof(1e10), TFloat);
		#end
		#if !eval
		eq(Type.typeof(1.0), TInt);
		#end
		var i0 = haxe.Int64.fromInt(256);
		eq(Type.typeof(i0), TInt64);
		var ibig = haxe.Int64.make(1,0);
		eq(Type.typeof(ibig), TInt64);

		// enumEq
		t(Type.enumEq(NoArgs, NoArgs));
		t(Type.enumEq(OneArg(1), OneArg(1)));
		t(Type.enumEq(RecArg(OneArg(1)), RecArg(OneArg(1))));
		t(Type.enumEq(MultipleArgs(1, "foo"), MultipleArgs(1, "foo")));
		f(Type.enumEq(NoArgs, OneArg(1)));
		f(Type.enumEq(NoArgs, RecArg(NoArgs)));
		f(Type.enumEq(NoArgs, MultipleArgs(1, "foo")));
		f(Type.enumEq(OneArg(1), OneArg(2)));
		f(Type.enumEq(RecArg(OneArg(1)), RecArg(OneArg(2))));
		t(Type.enumEq(EA, EA));
		f(Type.enumEq(EA, EB));

		// enumConstructor
		eq(Type.enumConstructor(NoArgs), "NoArgs");
		eq(Type.enumConstructor(OneArg(1)), "OneArg");
		eq(Type.enumConstructor(RecArg(OneArg(1))), "RecArg");
		eq(Type.enumConstructor(MultipleArgs(1, "foo")), "MultipleArgs");
		eq(Type.enumConstructor(EC), "EC");

		// enumParameters
		eq(Type.enumParameters(NoArgs).length, 0);
		eq(Type.enumParameters(OneArg(1))[0], 1);
		eq(Type.enumParameters(RecArg(NoArgs))[0], NoArgs);
		aeq(([1, "foo"] : Array<Dynamic>), Type.enumParameters(MultipleArgs(1, "foo")));
		eq(Type.enumParameters(EC).length, 0);

		// enumIndex
		eq(Type.enumIndex(NoArgs), 0);
		eq(Type.enumIndex(OneArg(1)), 1);
		eq(Type.enumIndex(RecArg(OneArg(1))), 2);
		eq(Type.enumIndex(MultipleArgs(1, "foo")), 3);
		eq(Type.enumIndex(EB), 1);

		// allEnums
		eq(Type.allEnums(E)[0], NoArgs);
		aeq([EBreak, EContinue], Type.allEnums(haxe.macro.Expr.ExprDef));
		aeq([EA, EB, EC], Type.allEnums(EnumFlagTest));


	}
}

@:keep private class ClassWithCtorDefaultValues {
	public var a:Null<Int>;
	public var b:String;

	public function new(a = 1, b = "foo") {
		this.a = a;
		this.b = b;
	}
}

private class ClassWithCtorDefaultValuesChild extends ClassWithCtorDefaultValues {}

@:keep private class ClassWithCtorDefaultValues2 {
	public var a:Null<Float>;
	public var b:String;

	public function new(a = 1.1, b = "foo") {
		this.a = a;
		this.b = b;
	}
}
