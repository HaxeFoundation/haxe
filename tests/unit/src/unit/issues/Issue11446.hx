package unit.issues;

typedef Td = E<String>;

private enum E<T> {
	C(s:String):Td;
}

private function match<T>(e:E<T>) {
	return switch (e) {
		case C(s): s;
	}
}

class Issue11446 extends Test {
	function test() {
		eq("foo", match(C("foo")));
	}
}
