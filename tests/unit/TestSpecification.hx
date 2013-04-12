package unit;

typedef T = {
	function func():Void;
	var v:String;
	public var prop(default, null):String;
}

@:keep class C {
	public function func() { }
	public var v:String;
	public var prop(default, null):String;
	static function staticFunc() { }
	static public var staticVar:String;
	static var staticProp(default, null):String;

	public function new() {
		v = "var";
		prop = "prop";
		staticVar = "staticVar";
		staticProp = "staticProp";
	}
}

@:keep class C2 {
	public function func() { return "foo"; }
	public var v:String;
	public var prop(default, null):String;
	@:isVar public var propAcc(get, set):String;

	public function new() {
		v = "var";
		prop = "prop";
		propAcc = "0";
	}
	
	public function get_propAcc() {
		return "1";
	}
	
	public function set_propAcc(v) {
		return this.propAcc = v.toUpperCase();
	}	
}

class CChild extends C { }

class CDyn extends C implements Dynamic { }

private class EmptyClass {
	public function new() { }
}

private class ClassWithToString {
	public function new() { }
	public function toString() return "ClassWithToString.toString()";
}

private class ClassWithToStringChild extends ClassWithToString {

}

private class ClassWithToStringChild2 extends ClassWithToString {
	public override function toString() return "ClassWithToStringChild2.toString()";
}

@:keep private class ClassWithCtorDefaultValues {
	public var a : Null<Int>;
	public var b : String;
	public function new(a = 1, b = "foo") {
		this.a = a;
		this.b = b;
	}
}

private class ClassWithCtorDefaultValuesChild extends ClassWithCtorDefaultValues {

}

private enum SomeEnum<T> {
	NoArguments;
	OneArgument(t:T);
}

private class IntWrap {
	public var i(default, null):Int;

	public function new(i:Int) {
		this.i = i;
	}

	static public function compare(a:IntWrap, b:IntWrap) {
		return if (a.i == b.i) 0;
			else if (a.i > b.i) 1;
			else -1;
	}
}

private enum E {
	NoArgs;
	OneArg(i:Int);
	RecArg(e:E);
	MultipleArgs(i:Int, s:String);
}

enum EnumFlagTest {
	EA;
	EB;
	EC;
}

#if !macro
@:build(unit.UnitBuilder.build("unitstd"))
#end
class TestSpecification extends Test {

}
