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
	static var staticVar:String;
	static var staticProp(default, null):String;
	
	public function new() {
		v = "var";
		prop = "prop";
		staticVar = "staticVar";
		staticProp = "staticProp";
	}
}

private class EmptyClass {
	public function new() { }
}

private class ClassWithToString {
	public function new() { }
	public function toString() return "ClassWithToString.toString()"
}

private class ClassWithToStringChild extends ClassWithToString {
	
}

private class ClassWithToStringChild2 extends ClassWithToString {
	public override function toString() return "ClassWithToStringChild2.toString()"
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

#if !macro
@:build(unit.UnitBuilder.build("unitstd"))
#end
class TestSpecification extends Test 
{

}
