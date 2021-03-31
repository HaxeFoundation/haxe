package unit.issues;

private abstract A(String) {
	public inline function new(s:String) {
		this = s;
	}

	public function f(index:Int) {
		return StringTools.fastCodeAt(this, index);
	}

	public inline function g(index:Int) {
		return StringTools.fastCodeAt(this, index);
	}
}

class Issue9543 extends unit.Test {
	#if cpp
	function testVar() {
		var foo:Int = 0;
		var bar:cpp.Reference<Int> = foo;
		foo += 1;
		eq(1, foo);
		eq(1, bar);
	}

	function testAssign() {
		var foo:Int = 0;
		var bar:cpp.Reference<Int>;
		bar = foo;
		foo += 1;
		eq(1, foo);
		eq(1, bar);
	}
	#end
}
