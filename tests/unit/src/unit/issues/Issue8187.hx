package unit.issues;

class Issue8187 extends unit.Test {
	static inline var enumValue = Leaf;
	function test() {
		eq(enumValue, Leaf);
	}
}

private enum Tree {
	Leaf;
	Node(a:Tree, b:Tree);
}
