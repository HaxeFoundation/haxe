package unit.spec;

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

class EmptyClass {
	public function new() { }
}

@:keep class ReallyEmptyClass { }

class ClassWithToString {
	public function new() { }
	public function toString() return "ClassWithToString.toString()";
}

class ClassWithToStringChild extends ClassWithToString {

}

class ClassWithToStringChild2 extends ClassWithToString {
	public override function toString() return "ClassWithToStringChild2.toString()";
}

@:keep class ClassWithCtorDefaultValues {
	public var a : Null<Int>;
	public var b : String;
	public function new(a = 1, b = "foo") {
		this.a = a;
		this.b = b;
	}
}

class ClassWithCtorDefaultValuesChild extends ClassWithCtorDefaultValues {

}

@:keep class ClassWithCtorDefaultValues2 {
	public var a : Null<Float>;
	public var b : String;
	public function new(a = 1.1, b = "foo") {
		this.a = a;
		this.b = b;
	}
}

enum SomeEnum<T> {
	NoArguments;
	OneArgument(t:T);
}

class IntWrap {
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

enum E {
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

enum EnumFlagTest2 {
	EF_00; EF_01; EF_02; EF_03; EF_04; EF_05; EF_06; EF_07;
	EF_08; EF_09; EF_10; EF_11; EF_12; EF_13; EF_14; EF_15;
	EF_16; EF_17; EF_18; EF_19; EF_20; EF_21; EF_22; EF_23;
	EF_24; EF_25; EF_26; EF_27; EF_28; EF_29; EF_30; EF_31;
}

enum EVMTest {
	EVMA;
	EVMB(?s:String);
	EVMC(s:String, ?i:Int);
	EVMD(n:EVMTest);
	EVME(?n:EVMTest);
	EVMF(a:Array<EVMTest>);
}

class NonRttiClass { }

@:rtti
@:keepSub
class RttiClass1 {
	static var v:String;
	public function f() {
		return 33.0;
	}
}

class RttiClass2 extends RttiClass1 { }

class RttiClass3 extends RttiClass1 {
	override function f():Int {
		return 33;
	}
}