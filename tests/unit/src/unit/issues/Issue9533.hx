package unit.issues;

class Issue9533 extends unit.Test {
	function test() {
		var unit = "fail";
		try throw new Error("success") catch (e:Error) unit = e.message;
		eq("success", unit);
	}
}

private class Error extends haxe.Exception {}
