package unit.issues;
import unit.Test;

abstract Lazy<T>(Void->T) {
	public function new(f) {
		this = f;
	}

	public function evaluate() {
		return this();
	}

    @:from static function ofConst<T>(c:T):Lazy<T> {
        return new Lazy(function() return c);
	}
}

class Issue2614 extends Test {

	function test() {
		var fInt = lazy(2);
		var fFloat = lazy(2.);
		eq(fInt.evaluate(), 2);
		feq(fFloat.evaluate(), 2.);
		unit.TestType.typedAs(fInt, (null : Lazy<Int>));
		unit.TestType.typedAs(fFloat, (null : Lazy<Float>));
	}

	static public function lazy<A>(l:Lazy<A>):Lazy<A> {
        return l;
	}
}