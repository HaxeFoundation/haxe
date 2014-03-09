package unit.issues;
import unit.Test;

private enum E {
	A(d:Dynamic);
	B;
}

class Issue2728 extends Test {
	function test() {
		var o = { foo: 12 };
		var a = A(o);
		var v = null;
		switch(a) {
			case A(d):
				v = d;
			case B:
		}
		eq(o, v);
	}
}