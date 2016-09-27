package unit.issues;
import unit.Test;
using unit.issues.misc.Issue2720Macro;

class Issue2720 extends unit.Test {
	function test() {
		eq("foofoo", "foo".dupe());
	}
}