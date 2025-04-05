package unit.issues;

class Issue8243 extends unit.Test {
	var involveRecursiveAbstractTyping:Null<Rec> = null;

	function test() {
		t(involveRecursiveAbstractTyping == null);
	}
}

private abstract Rec(Array<Rec>) from Array<Rec> {}