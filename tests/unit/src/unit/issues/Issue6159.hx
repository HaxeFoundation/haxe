package unit.issues;

class Issue6159 extends unit.Test {
    static var e = Dummy;

    function test() {
        eq(A, e.A);
    }
}

private enum Dummy {
	A;
	B;
}