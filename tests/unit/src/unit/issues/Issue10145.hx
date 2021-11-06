package unit.issues;

// whatever
#if !erase_generics
private interface I<T> {
	function get():T;
}

private class C<T> implements I<T> {
	public var value:T;

	public function new(value:T) {
		this.value = value;
	}

	public function get():T {
		return value;
	}
}

@:multiType(T)
private abstract A<T>(I<T>) {
	public function new(value:T);

	@:to static function ofInt(_:I<Int>, value:Int):C<Int> {
		return new C(value);
	}

	@:to function toT():T {
		return this.get();
	}
}
#end

class Issue10145 extends unit.Test {
	#if !erase_generics
	function test() {
		var x = new A(10);
		var y:Int = x;
		eq(10, y);
	}
	#end
}
