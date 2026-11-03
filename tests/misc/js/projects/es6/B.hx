extern class A {
	function new();
}

class B extends A {
	function new() {
		trace(this);
		super();
	}
}
