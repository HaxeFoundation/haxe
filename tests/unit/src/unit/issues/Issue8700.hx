package unit.issues;

private enum abstract JsonTypeKind<T>(String) {
	var TMono;
}

class Issue8700 extends unit.Test {
	function test() {
		eq("unit.issues._Issue8700.JsonTypeKind<Unknown<0>>", HelperMacros.typeString(TMono));
	}
}