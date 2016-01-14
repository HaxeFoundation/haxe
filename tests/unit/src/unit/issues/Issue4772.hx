package unit.issues;

class Issue4772 extends Test {
	macro static function testMacro(){
		var a : Null<Bool> = null;
		var b = a && true;
		return macro $v{b};
	}

	function test() {
		f(testMacro());
	}
}