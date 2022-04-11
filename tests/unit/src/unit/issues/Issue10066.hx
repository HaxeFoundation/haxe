package unit.issues;

private abstract Foo(Array<String>) from Array<String> {
	@:op(_ += _) inline function add(string:String)
		this.push(string);
}

class Issue10066 extends unit.Test {
	function testExpr() {
		var a = [];
		var foo:Foo = a;
		foo += (1 == 2 ? 'two' : 'three');
		eq("three", a[0]);
	}

	static var staticFoo:Foo;

	function testField() {
		var a = [];
		staticFoo = a;
		staticFoo += (1 == 2 ? 'two' : 'three');
		eq("three", a[0]);
	}
}
