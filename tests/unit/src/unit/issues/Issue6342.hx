package unit.issues;

class Issue6342 extends unit.Test {
	@:analyzer(no_optimize)
	function test() {
		var t = new A(1);
		var tmp1 = t.foo().x;
		eq(1, tmp1);
		
		var tmp2 = ({
			new A(2);
		}).x;
		eq(2, tmp2);
	}
}

@:keep
private class A
{
	public var x:Int;
	
	@:analyzer(no_optimize)
	public inline function new(x) {
		this.x = x;
	}
	
	@:analyzer(no_optimize)
	public inline function foo() {
		var tmp = x;
		return new A(x);
	}
}