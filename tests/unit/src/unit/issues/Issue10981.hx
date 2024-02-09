package unit.issues;

import haxe.EnumFlags;

private enum E {
	A;
	B;
}

class Issue10981 extends Test {
	function test() {
		var flags:EnumFlags<E> = A | B;
		eq(3, flags.toInt());
	}
}
