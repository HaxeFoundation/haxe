package unit.issues;

class Issue3448 extends Test {
	function test() {
		t(f1() != null);
	}

    static function f1(?pos) {
       return f2(pos);
    }

	static function f2(?p:haxe.PosInfos) {
		return p;
	}
}