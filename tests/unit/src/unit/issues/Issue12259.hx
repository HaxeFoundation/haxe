package unit.issues;

import unit.issues.misc.issue12259.Element;

@:access(unit.issues.misc.issue12259.Element)
class Issue12259 extends unit.Test {
	public function test() {
		var elt = new Element();
		eq(true, elt.foo);
		eq(true, elt.foo = false);
	}
}
