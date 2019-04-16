package unit.issues;

private class A {
    private var x = 1;
}

class Issue7388 extends unit.Test {
	function test() {
		eq("unit.issues._Issue7388.A does not have a constructor", HelperMacros.getErrorMessage(A.new));
	}
}