extern class A {
	function new(v:Any);
}

class B extends A {
	function new() {
		super(this);
	}
}
