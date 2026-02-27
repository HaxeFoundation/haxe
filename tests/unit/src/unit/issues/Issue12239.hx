package unit.issues;

private typedef BinaryTreeDef = {
	final ?l:Null<BinaryTree12239>;
	final ?r:Null<BinaryTree12239>;
}

@:forward private abstract BinaryTree12239(BinaryTreeDef) {
	public function new(self:BinaryTreeDef) this = self;
	@:noUsing static public function lift(self:BinaryTreeDef):BinaryTree12239 return new BinaryTree12239(self);
}

class Issue12239 extends Test {
	#if hl
	function test() {
		var bt = BinaryTree12239.lift({});
		t(bt != null);
	}
	#end
}
