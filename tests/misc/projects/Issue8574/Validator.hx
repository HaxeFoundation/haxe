class Validator {
	static public function check(a1:Array<Int>, a2:Array<Int>) {
		if(a1.length != a2.length) {
			fail(a1, a2);
		}
		for(i in 0...a1.length) {
			if(a1[i] != a2[i]) {
				fail();
			}
		}
	}

	static function fail(a1:Array<Int>, a2:Array<Int>) {
		Sys.stderr().writeString('Arrays are not equal: $a1 != $a2\n');
		Sys.exit(1);
	}
}