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

private abstract MultiSetter(Array<String>) {
	public function new() {
		this = [];
	}

	@:arrayAccess public function setInt(k:String, v:Int):Int {
		this.push('$k:int:$v');
		return v;
	}

	@:arrayAccess public function setString(k:String, v:String):String {
		this.push('$k:str:$v');
		return v;
	}

	public function toString():String {
		return this.join(",");
	}
}

private abstract GenericSetter(Array<String>) {
	public function new() {
		this = [];
	}

	@:arrayAccess public function set<T>(k:String, v:Array<T>):Array<T> {
		this.push('$k:${v.length}');
		return v;
	}

	public function toString():String {
		return this.join(",");
	}
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

	function testMultiSetter() {
		var m = new MultiSetter();
		m["a"] = 42;
		m["b"] = "hello";
		eq("a:int:42,b:str:hello", m.toString());
	}

	function testGenericSetter() {
		var g = new GenericSetter();
		g["x"] = [];
		g["y"] = [1, 2, 3];
		eq("x:0,y:3", g.toString());
	}
}
