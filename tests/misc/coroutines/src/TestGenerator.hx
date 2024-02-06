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

private typedef Yield<T> = Coroutine<T->Void>;

private function sequence<T>(f:Coroutine<Yield<T>->Void>):Iterator<T> {
	var finished = false;
	var nextValue:T = null;

	var nextStep = null;

	function finish(_, _) {
		finished = true;
	}

	@:coroutine function yield(value:T) {
		nextValue = value;
		Coroutine.suspend(cont -> nextStep = cont);
	}

	function hasNext():Bool {
		if (nextStep == null) {
			nextStep = f.create(yield, finish);
			nextStep(null, null);
		}
		return !finished;
	}

	function next():T {
		var value = nextValue;
		nextStep(null, null);
		return value;
	}

	return {hasNext: hasNext, next: next};
}

private typedef Tree<T> = {
	var leaf:T;
	var ?left:Tree<T>;
	var ?right:Tree<T>;
}
