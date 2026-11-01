package unit.issues;

// hl wil fail with Out of memory here (see #12955)
#if !hl
private typedef BinaryTreeDef<T> = {
	public final ?l:Null<BinaryTree<T>>;
	public final ?r:Null<BinaryTree<T>>;
}

@:forward private abstract BinaryTree<T>(BinaryTreeDef<T>) {
	public function new(self:BinaryTreeDef<T>) this = self;

	@:noUsing static public function lift<T>(self:BinaryTreeDef<T>):BinaryTree<T>
		return new BinaryTree(self);
}
#end

class Issue12239 extends Test {
	function test() {
		#if hl
		noAssert();
		#else
		var tree = BinaryTree.lift({});
		t(tree != null);
		#end
	}
}
