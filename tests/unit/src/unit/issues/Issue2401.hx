package unit.issues;
import unit.Test;

private abstract Foo(Int) from Int {
    public function toString()
        return 'Foo: $this';
}

class Issue2401 extends Test {
	function test() {
		eq("Foo: 1", Std.string((1 : Foo)));
	}
}