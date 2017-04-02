package unit.issues;

class Issue6070 extends unit.Test {
	static var C = ["1", "2"];
	static var B = C.concat(["3", "4"]);

	function test():Void {
		eq("1234", B.join(""));
	}
}