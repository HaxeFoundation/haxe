package unit.issues;

using unit.issues.misc.Issue2497Class;

private class Object1 {
	public var sayHelloTo: String -> ?String -> String;
	public function new() {
		sayHelloTo = "hello".addTail;
	}
}



class Issue2497 extends Test {
	function test() {
		var obj1 = new Object1();
		eq("hello world !", obj1.sayHelloTo("world"));
	}
}