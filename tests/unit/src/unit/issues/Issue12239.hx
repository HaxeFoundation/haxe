package unit.issues;

private typedef BinaryTreeDef<T> = {
	public final ?l:Null<BinaryTree<T>>;
	public final ?r:Null<BinaryTree<T>>;
}

@:forward private abstract BinaryTree<T>(BinaryTreeDef<T>) {
	public function new(self:BinaryTreeDef<T>) this = self;

	@:noUsing static public function lift<T>(self:BinaryTreeDef<T>):BinaryTree<T>
		return new BinaryTree(self);
}

class Issue12239 extends Test {
	function test() {
		var tree = BinaryTree.lift({});
		t(tree != null);
	}
}
