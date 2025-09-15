import haxe.coro.schedulers.IScheduleObject;
import haxe.Int64;
import haxe.coro.schedulers.Scheduler;
import hxcoro.task.CoroTask;
import haxe.coro.context.Context;
import haxe.Exception;

private typedef Yield<T> = Coroutine<T->Void>;

private function sequence<T>(f:Coroutine<Yield<T>->Void>):Iterator<T> {
	var hasValue = false;
	var nextValue:T = null;
	var exception:Null<Exception> = null;

	var nextStep = null;
	final scope = new CoroTask(Context.create(new ImmediateScheduler()), CoroTask.CoroScopeStrategy);

	@:coroutine function yield(value:T) {
		nextValue = value;
		hasValue = true;
		suspend(cont -> {
			nextStep = () -> {
				hasValue = false;
				cont.resume(null, null);
				if (!scope.isActive()) {
					exception = scope.getError();
				}
			}
		});
	}

	nextStep = () -> {
		f(yield, scope);
		scope.start();
	}

	function hasNext() {
		nextStep();
		if (exception != null) {
			throw exception;
		}
		return hasValue;
	}
	function next() {
		return nextValue;
	}

	return {hasNext: hasNext, next: next};
}

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

	function testException() {
		final result = [];
		Assert.raises(() -> {
			for (i in sequence(yield -> {
				yield(1);
				yield(2);
				throw "oh no";
				yield(3);
			})) {
				result.push(i);
			}
		});
		Assert.same([1, 2], result);
	}
}

private typedef Tree<T> = {
	var leaf:T;
	var ?left:Tree<T>;
	var ?right:Tree<T>;
}
