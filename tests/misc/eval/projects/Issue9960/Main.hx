function main() {
	var v = Foo;
	v.foo();
	if (v != Bar) {
		throw 'Expected 1, got $v';
	}
}

enum abstract Val(Int) {
	var Foo = 0;
	var Bar = 1;

	public inline function foo()
		this = this + 1;
}
