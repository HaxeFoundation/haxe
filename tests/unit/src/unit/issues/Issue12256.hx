package unit.issues;

private typedef Tree<T> = {
	var left:Tree<T>;
}

class Issue12256 extends Test {
	function test() {
		function iterTree<T>(tree:Tree<T>):Tree<T> {
			return tree == null ? null : tree.left;
		}
		eq(null, iterTree(null));
		var tree:Tree<Int> = {left: null};
		tree.left = tree;
		t(iterTree(tree) == tree);
	}
}
