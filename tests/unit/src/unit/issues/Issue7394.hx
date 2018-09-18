package unit.issues;

private typedef Foo = {
	final i:Int;
}

@:forward
private abstract Bar(Foo) from Foo {
	@:op(A > B) function compare(other:Bar)
		return this.i > other.i;
}

class Issue7394 extends unit.Test {
	function test() {
		var b:Bar = {i: 0};
		f(b > {i: 1});
	}
}