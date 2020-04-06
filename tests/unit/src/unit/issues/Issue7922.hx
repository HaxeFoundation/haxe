package unit.issues;

import utest.Assert;

class Issue7922 extends unit.Test {
	function test() {
		var a:A = LAME;
		var b:B = HOT;

		switch (a) {
			case LAME | COOL:
		}

		switch (b) {
			case NOT | HOT:
		}
		Assert.pass();
	}
}

private enum abstract A(Int) to Int {
	var LAME =  0;
	var COOL =  1;
}

private enum abstract B(Int) {
	var NOT = LAME;
	var HOT = COOL;
}