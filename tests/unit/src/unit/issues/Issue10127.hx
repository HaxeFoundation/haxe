package unit.issues;

@:allow(unit.issues.Issue10127)
class Issue10127 extends Test {
	static final privateField = true;

	function test() {
		accessible();
		noAssert();
	}
}

function accessible() {
	Issue10127.privateField;
}