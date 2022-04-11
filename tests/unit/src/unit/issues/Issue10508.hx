package unit.issues;

class Issue10508 extends Test {
	#if java
	function test() {
		t(java.math.RoundingMode.getConstructors().length > 0);
		t(java.math.RoundingMode.createAll().length > 0);
	}
	#end
}
