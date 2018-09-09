package unit.issues;

private enum abstract A1(Int) to Int {
    var First;
    var Second;
    var Fifth = 5;
    var Sixth;
}

private enum abstract A2(String) to String {
    var FirstString;
    var SecondString;
}

abstract MyInt(Int) from Int to Int { }

private enum abstract A3(MyInt) to Int {
	var Zero;
    var Ninth = 9;
	var Tenth;
}

class Issue7139 extends unit.Test {
	function test() {
		eq(0, First);
		eq(1, Second);
		eq(5, Fifth);
		eq(6, Sixth);

		eq("FirstString", FirstString);
		eq("SecondString", SecondString);

		eq(0, Zero);
		eq(9, Ninth);
		eq(10, Tenth);
	}
}