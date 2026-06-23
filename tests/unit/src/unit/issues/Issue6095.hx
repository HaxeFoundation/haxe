package unit.issues;

class Issue6095 extends unit.Test {
	var a(get, null):String;

	function get_a(?pos:haxe.PosInfos):String {
		return a;
	}

	var b(get, set):Int;
	var _b:Int = 0;

	function get_b(?pos:haxe.PosInfos):Int {
		return _b;
	}

	function set_b(v:Int, ?pos:haxe.PosInfos):Int {
		_b = v;
		return v;
	}

	function test() {
		a = "12345";
		eq("12345", a);

		b = 7;
		eq(7, b);
	}
}
