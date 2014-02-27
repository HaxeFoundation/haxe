package unit.issues;

enum Tree<T> {
    Leaf(v:T);
    Node(l:Tree<T>, r:Tree<T>);
}

class Issue2508 extends unit.Test {

	public function test() {
		t(unit.TestType.typeError(
			switch(Leaf("foo")) {
				case Leaf(_)
				   | Leaf("foo"): // This pattern is unused
				case Node(l,r):
			}
		));
	}

}