package unit.issues;

class Issue4337 extends Test {

	function test() {
		eq("goto", goto());
	}

	static function goto(){
		return "goto";
	}

}