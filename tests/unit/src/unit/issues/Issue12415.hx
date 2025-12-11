package unit.issues;

using unit.issues.misc.Issue12415Abstract;

class Issue12415 extends Test {
	function test() {
		var value:Issue12415Abstract = null;
		eq(true, value == null);
		eq(false, value != null);
		eq(true, value.isNull());

		#if static
		eq(true, (cast value) == 0);

		#if !flash
		eq(true, haxe.Int64.isZero((null:Issue12415Abstract)));
		#end
		#end
	}
}
