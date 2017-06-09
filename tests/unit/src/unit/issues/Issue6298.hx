package unit.issues;
class Issue6298 extends unit.Test {
	public function test(){
		var tst = new Some ();
		var ddd = "SOME";
		eq (tst.testCall (ddd), ddd);
	}
}

class Test {
	public static function good (s : String) : String {
		return s;
	}
}

class Some {
	var call : String -> String;

	public function new () {
		call = Test.good;
	}

	public function testCall (s : String) : String {
		return call (s);
	}
}

