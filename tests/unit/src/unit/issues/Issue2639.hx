package unit.issues;
import unit.Test;

typedef Measurable = {
	public var length(default, null):Int;
}

private abstract A<T>(Int) {
	public function new(i) this = i;
	@:from static public inline function fromT<T:Measurable>(t:T) {
		return new A(t.length);
	}

	public function get() {
		return this;
	}
}

class Issue2639 extends Test {
	public function test() {
		var a:A<Int>;
		t(unit.HelperMacros.typeError(a = 0));
		var b:A<String> = "foo";
		eq(3, b.get());
	}
}