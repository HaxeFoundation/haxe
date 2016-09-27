package unit.issues;
import unit.Test;

private extern class Ext {
	static inline var v = "foo";

	public function new() { }

	public inline function test():Void { }

	static inline function test2():String {
		return v;
	}
}

class Issue2813 extends Test {
	function test() {
		var f = Ext.test2;
		eq("foo", f());

		t(unit.HelperMacros.typeError({
			var e = new Ext();
			var f = e.test;
		}));
	}
}