package issues;

private class A {
	public var x : Int = 1;
	public inline function new () {};
}

private class B extends A {
	public var y : Int = 2;
	public inline function new () { super(); };
}

class Issue6093 {
	@:js('
		var a_y;
		var a_x;
		a_y = 2;
		a_x = 1;
	')
	@:analyzer(ignore)
	static public function main() {
		var a = new B();
	}
}