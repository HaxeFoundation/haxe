package hxcoro.task;

import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.coro.context.Context;
import hxcoro.task.ICoroTask;

class CoroScopeTask<T, C = Any> extends CoroTask<T, C> {
	final parent:Null<AbstractTask<Any>>;

	public function new(context:Context) {
		super(context);
		// slightly subtle: context here refers to the incoming context which still holds the parent
		parent = context.get(CoroTask.key);
		if (parent != null) {
			parent.addChild(this);
		}
	}

	function childSucceeds(_:AbstractTask<C>) {}

	function childErrors(_:AbstractTask<C>, error:Exception) {
		if (this.error == null) {
			this.error = error;
			cancel();
		}
	}

	function childCancels(_:AbstractTask<C>, cause:CancellationException) {}

	function complete() {
		parent?.childCompletes(this, false);
		handleAwaitingContinuations();
	}
}

class StartableCoroScopeTask<T, C = Any> extends CoroScopeTask<T, C> implements IStartableCoroTask<T> {
	final lambda:NodeLambda<T, C>;

	/**
		Creates a new task using the provided `context` in order to execute `lambda`.
	**/
	public function new(context:Context, lambda:NodeLambda<T, C>) {
		super(context);
		this.lambda = lambda;
	}

	/**
		Starts executing this task's `lambda`. Has no effect if the task is already active or has completed.
	**/
	override function doStart() {
		super.doStart();
		runNodeLambda(lambda);
	}
}
