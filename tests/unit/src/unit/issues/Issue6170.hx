package unit.issues;

class Issue6170 extends unit.Test {
	function test() {
		var a:Array<String> = getNull();
		var b:Array<Dynamic> = a;
		eq(null, b);
	}

	@:pure(false)
	static function getNull():Array<String> {
		return null;
	}
}
