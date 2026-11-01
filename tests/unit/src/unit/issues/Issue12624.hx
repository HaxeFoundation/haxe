package unit.issues;

private enum abstract State(Int) {
	var Empty = 0;
	var Inquisition = 1;
}

class Issue12624 extends Test {
	var s1 : Null<State> = null;
	var s2 : Null<State> = null;
	function test() {
		var r : State = s1 ?? s2 ?? Inquisition;
		t(r == Inquisition);
		var r = s1 ?? s2 ?? Inquisition;
		t(r == Inquisition);
	}
}
