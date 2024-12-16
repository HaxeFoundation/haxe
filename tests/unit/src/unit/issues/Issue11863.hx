package unit.issues;

private enum E {
	C(r:R);
}

private typedef R = {
	f:Null<E>
}

class Issue11863 extends Test {
	function checkIdentity(e:E) {
		switch (e) {
			case C(r1):
				return (e == r1.f);
		}
		return false;
	}

	function test() {
		var r = {
			f: null
		};
		var e = C(r);
		r.f = e;
		t(checkIdentity(e));
		var e2 = haxe.runtime.Copy.copy(e);
		t(checkIdentity(e2));
	}
}
