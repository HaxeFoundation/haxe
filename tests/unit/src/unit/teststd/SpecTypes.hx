package unit.teststd;

/**
	Shared helper types used across multiple standard library tests.
**/
typedef T = {
	function func():Void;
	var v:String;
	public var prop(default, null):String;
}

@:keep class C {
	public function func() {}

	public var v:String;
	public var prop(default, null):String;
	static function staticFunc() {}
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
	public function func() {
		return "foo";
	}

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

class CChild extends C {}

class EmptyClass {
	public function new() {}
}

@:keep class ReallyEmptyClass {}

class ClassWithToString {
	public function new() {}

	public function toString()
		return "ClassWithToString.toString()";
}

class ClassWithToStringChild extends ClassWithToString {}

class ClassWithToStringChild2 extends ClassWithToString {
	public override function toString()
		return "ClassWithToStringChild2.toString()";
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

enum EVMTest {
	EVMA;
	EVMB(?s:String);
	EVMC(s:String, ?i:Int);
	EVMD(n:EVMTest);
	EVME(?n:EVMTest);
	EVMF(a:Array<EVMTest>);
}
