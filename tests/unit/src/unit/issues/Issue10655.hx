package unit.issues;

import utest.Assert;

@:structInit
private class Student {
	public final name:String;
	public final score:Int;
}

private abstract A(String) {
	public function new() {
		this = "";
	}

	@:arrayAccess public function arrayWrite(k:String, v:Array<Dynamic>) {}
}

class Issue10655 extends Test {
	function test() {
		var map = new Map<String, Student>();

		var student1:Student = {name: "Boris", score: 20};
		map["the best"] = student1;

		map["the worst"] = {name: "Vianney", score: -1};
		Assert.pass();
	}

	function test2() {
		var a = new A();
		a["foo"] = [1, "foo"];
		Assert.pass();
	}
}
