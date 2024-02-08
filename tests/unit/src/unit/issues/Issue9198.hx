package unit.issues;

import unit.Test;

class Issue9198 extends Test {
	public function test() {
		eq(null, Std.parseInt("axolotl"));
	}
}
