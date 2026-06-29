package unit.issues;

// Recursive typedef used through a generic function. On the hl target the
// recursive structure made `Hlcode.tsame` recurse forever (compiler hang).
private typedef Tree<T> = {
	var left:Tree<T>;
}

class Issue12256 extends Test {
	function test() {
		function iterTree<T>(tree:Tree<T>):Null<T> {
			return null;
		}
		t(iterTree(null) == null);
	}
}
