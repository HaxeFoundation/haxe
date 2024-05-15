import yield.Yield;

class TestGenerator extends utest.Test {
	function testSimple() {
		var iter = sequence(yield -> {
			yield(1);
			yield(2);
			yield(3);
		});
		Assert.same([1,2,3], [for (v in iter) v]);
	}

	function testTreeIter() {
		@:coroutine function iterTreeRec<T>(yield:Yield<T>, tree:Tree<T>) {
			yield(tree.leaf);
			if (tree.left != null) iterTreeRec(yield, tree.left);
			if (tree.right != null) iterTreeRec(yield, tree.right);
		}

		function iterTree<T>(tree:Tree<T>):Iterator<T> {
			return sequence(yield -> iterTreeRec(yield, tree));
		}

		var tree:Tree<Int> = {
			leaf: 1,
			left: {
				leaf: 2,
				left: {leaf: 3},
				right: {leaf: 4, left: {leaf: 5}},
			},
			right: {
				leaf: 6,
				left: {leaf: 7}
			}
		};

		Assert.same([1,2,3,4,5,6,7], [for (v in iterTree(tree)) v]);
	}
}

#if hl
// hl has some problem when dealing with recursive anon, use a class to bypass
@:structInit class Tree<T> {
	public var leaf:T;
	@:optional public var left:Tree<T>;
	@:optional public var right:Tree<T>;
}
#else
private typedef Tree<T> = {
	var leaf:T;
	var ?left:Tree<T>;
	var ?right:Tree<T>;
}
#end
