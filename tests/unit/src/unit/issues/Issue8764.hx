package unit.issues;

class Issue8764 extends unit.Test {
#if (static && !jvm)
	function test() {
		eq(0.0, foo());
	}

	static function foo(?e:Float):Null<Single> {
		return (e:Float);
	}
#end
}