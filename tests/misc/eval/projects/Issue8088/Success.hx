class Success {
	static var fail = false;

	static function main() {
		test('a', 'b', 's', [1, 2]);
		if(fail) Sys.exit(1);
	}

	static function test<T:String>(a:T, b:T, s:String, arr:Array<Int>) {
		check(true, a < b);
		check(true, a < s);
		check(true, s > a);
		check('sa', s + a);
		check('ab', a + b);
		check('as', a + s);
		check('sa', s + a);
		check('a[1,2]', a + arr);
		check('[1,2]a', arr + a);
	}

	static function check<T>(expected:T, actual:T, ?p:haxe.PosInfos) {
		if( expected != actual) {
			fail = true;
			Sys.stderr().writeString('${p.fileName}:${p.lineNumber}: $expected expected, but got $actual\n');
		}
	}
}