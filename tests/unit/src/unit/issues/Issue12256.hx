package unit.issues;

private typedef Tree = {
	var left:Tree;
}

class Issue12256 extends Test {
	function test() {
		function iterTree(tree:Tree) {
			return null;
		}
		iterTree(null);
		noAssert();
	}
}
