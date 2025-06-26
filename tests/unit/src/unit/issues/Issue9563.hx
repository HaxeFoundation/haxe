package unit.issues;

import unit.Test;
import utest.Assert;

enum abstract Noise(Null<Dynamic>) {
	final Noise = null;
}

class Issue9563 extends Test {
	function test() {
		switch Noise { // Unmatched patterns: _
			case Noise:
		}
		Assert.pass();
	}
}
