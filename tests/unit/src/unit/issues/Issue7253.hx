package unit.issues;

#if (cpp && debug) // debug enables HXCPP_CHECK_POINTER
private interface ISomeInterface {
	function getSomeProperty():Float;
}

private class SomeClass {
	var nullPointer:ISomeInterface;

	public var someProperty(get, null):Float;

	public function new() {}

	function get_someProperty():Float {
		return nullPointer.getSomeProperty();
	}
}
#end

class Issue7253 extends unit.Test {
	#if (cpp && debug)
	function test() {
		var inst = new SomeClass();
		exc(() -> inst.someProperty);
		exc(() -> Reflect.getProperty(inst, "someProperty"));
	}
	#end
}
