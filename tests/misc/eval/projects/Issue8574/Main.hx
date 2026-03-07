class Main {
	static function main() {
		var a = [];
		for(i in #if noOpt 0...three() #else 0...3 #end) {
			a.push(i);
			i++;
			a.push(i);
		}
		check([0,1, 1,2, 2,3], a);
	}

	@:pure(false)
	static public function three():Int {
		return 3;
	}

	static function check(a1:Array<Int>, a2:Array<Int>) {
		if(a1.length != a2.length) {
			fail(a1, a2);
		}
		for(i in 0...a1.length) {
			if(a1[i] != a2[i]) {
				fail(a1, a2);
			}
		}
	}

	static function fail(a1:Array<Int>, a2:Array<Int>) {
		Sys.stderr().writeString('Arrays are not equal: $a1 != $a2\n');
		Sys.exit(1);
	}
}