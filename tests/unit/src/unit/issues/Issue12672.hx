package unit.issues;

class Issue12672 extends Test {
#if jvm
	function test() {
		var a = new unit.issues.misc.issue12672.a.Foo();
		var b = new unit.issues.misc.issue12672.b.Foo();
		var fa = a.getValue;
		var fb = b.getValue;
		eq(1, fa());
		eq(2, fb());
		var fsa = unit.issues.misc.issue12672.a.Foo.getStaticValue;
		var fsb = unit.issues.misc.issue12672.b.Foo.getStaticValue;
		eq(1, fsa());
		eq(2, fsb());
	}
#end
}
