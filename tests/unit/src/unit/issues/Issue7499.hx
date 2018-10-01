package unit.issues;

class Issue7499 extends unit.Test {
	@:analyzer(ignore)
	function test() var a;

	@:analyzer(ignore)
	static function __init__() {
		var s:String;
	}

}