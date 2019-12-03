package unit.issues;

class Issue8975 extends unit.Test {
	function test() {
		eq('Alpha10', Reflect.callMethod(this, doIt, ['Alpha']));
	}

	function doIt(prefix:String, suffix:Int = 10):String {
		return prefix + suffix;
	}
}