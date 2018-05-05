package unit.issues;

using haxe.Int64Helper;

class Issue5493 extends Test {
	function test() {
		t(Int64Helper.parseString('1') == 1);
		t(Int64Helper.parseString('10') == 10);
		t(Int64Helper.parseString('-1') == -1);
		t(Int64Helper.parseString('-10') == -10);
	}
}
