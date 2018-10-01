package unit.issues;

class Issue7499 extends unit.Test {
	@:analyzer(ignore)
	function test() var a;

	#if !cppia
	@:analyzer(ignore)
	static function __init__() {
		var s:String;
	}
	#end
}