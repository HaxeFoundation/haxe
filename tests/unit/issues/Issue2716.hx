package unit.issues;
import unit.Test;

class Issue2716 extends Test {
	function test() {
		eq("null", Std.string(['foo', null][Std.random(1)+1]));
		eq("null", Std.string(['foo'][Std.random(1)+1]));
	}
}