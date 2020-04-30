package unit.issues;

private enum List<T> {
	Cons(element:T, next:List<T>);
	Nil;
}

class Issue8959 extends unit.Test {
	function test() {
		var list = Cons(1, Cons(2, Cons(3, Nil)));
		var acc = 0;

		while (true) {
			switch list {
				case Cons(element, next):
					list = next;
					acc = acc + element;
				case Nil:
					break;
			}
		}

		eq(6, acc);
	}
}