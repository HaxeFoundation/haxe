package unit.issues;

class Issue9897 extends unit.Test {
	static var nullValue:Null<Float> = null;
	static var floatValue:Null<Float> = 100;

	function test() {
		t(nullValue != floatValue);
		t(floatValue != nullValue);
	}
}
