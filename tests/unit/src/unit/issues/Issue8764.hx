package unit.issues;

class Issue8764 extends unit.Test {
#if static
	function test() {
		eq(0.0, foo());
	}

	static function foo(?e:Int):Null<Float> {
		return (e:Int);
	}
#end
}