package unit.issues;

class Issue3263 extends Test {
	public static function teststrings1 () {
		var str1 = 'Test';
		var len = str1.length;
		var str2 = str1.substr(0, -(len)-1);
		return str2;
	}

	public static function teststrings2 () {
		var str1 = 'Test';
		var str2 = str1.substr(0, -(str1.length)-1);
		return str2;
	}

	public static function teststrings3 () {
		var str1 = 'Test';
		var str2 = str1.substr(0, (str1.length)-1);
		return str2;
	}

	public static function teststrings4 () {
		var str1 = 'Test';
		var str2 = str1.substr(0, (str1.length));
		return str2;
	}

	public static function teststrings5 () {
		var str1 = 'Test';
		var str2 = str1.substr(0, str1.length);
		return str2;
	}

	public static function teststrings6 () {
		var str1 = 'Test';
		var str2 = str1.substr(0, -str1.length - 1);
		return str2;
	}

	public static function teststrings7 () {
		var str1 = 'Test';
		var str2 = str1.substr(0, -str1.length);
		return str2;
	}

	function test() {
		//eq("Tes", teststrings1());
		eq("Tes", teststrings3());
		eq("Test", teststrings4());
		eq("Test", teststrings5());
		//eq("Tes", teststrings2());
		//eq("Tes", teststrings6());
		eq("", teststrings7());
	}
}