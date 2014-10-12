package unit.issues;

private enum T {
    T1(x:Null<T>);
    T2(x:Int);
}

class Issue2580 extends Test {
	function test() {
		function match(t) {
			return switch (t) {
				case T1(T1(_)): 0;
				case T1(T2(_)): 1;
				case T1(null): 2;
				case T2(_): 3;
			}
		}
		eq(0, match(T1(T1(null))));
		eq(1, match(T1(T2(1))));
		eq(2, match(T1(null)));
		eq(3, match(T2(2)));
	}
}