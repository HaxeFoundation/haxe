package unit.issues;

class Issue8872 extends unit.Test {
	function test() {
		var d:Array<Dynamic> = [];
		d.push(getTrue() ? (0 : Dynamic) : ("foo" : Dynamic));
		eq(0, d[0]);
	}

	static function getTrue() {
		return true;
	}
}