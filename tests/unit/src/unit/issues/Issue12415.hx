package unit.issues;

using unit.issues.misc.Issue12415Abstract;

class Issue12415 extends Test {
	function test() {
		var value:Issue12415Abstract = null;
		eq(#if static haxe.Int64.ofInt(0) #else null #end, value);
		eq(true, value == null);
		eq(false, value != null);
		eq(true, value.isNull());
	}
}
