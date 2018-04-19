package unit;

using unit.TestUsing.MyUsingClass;
using override unit.TestUsing.MyDefaultUsingClass;
using unit.TestUsing.MyUsingClass;

class MyDefaultUsingClass {
	static public function charAt(s:String, i:Int) {
		return @:noUsing s.toUpperCase().charAt(i);
	}

	static public function testUsing(s:String) {
		return 0;
	}
}

class MyUsingClass {
	static public function testUsing(s:String) {
		return 1;
	}
}

class TestUsing extends Test {
	function test() {
		eq("F", "foo".charAt(0));
		eq(0, "foo".testUsing());
	}
}