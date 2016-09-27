package unit.issues;

private abstract A({}) from {} {
	@:from static function fromB(b:{a:Int}):A {
		return null;
	}
}

class Issue4843 extends Test {
	function test() {
		var a:A = {};
	}
}