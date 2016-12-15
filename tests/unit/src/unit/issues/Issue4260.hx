package unit.issues;

class Issue4260 extends unit.Test {
	var floatValue:Float = 0.0;
	var intValue:Float = 0;
	var nullValue:Null<Float>;

	function test() {
        t(intValue == floatValue);
		f(intValue == nullValue);
		f(floatValue == nullValue);
	}
}
