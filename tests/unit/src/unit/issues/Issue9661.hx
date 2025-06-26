package unit.issues;
import unit.Test;
import unit.issues.misc.Issue9661Macro;

private typedef Foo = {
	function make<T>(v:T):Foo;
}

class Issue9661 extends Test {
	public function test() {
		var foo:Foo = {make: null}
		Issue9661Macro.run(foo.make != null);
		utest.Assert.pass();
	}
}