package hxcoro;

import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.coro.context.Context;
import hxcoro.ICoroTask;

class CoroScopeTask<T> extends CoroTask<T> {
	final parent:Null<AbstractTask<Any>>;

	public function new(context:Context) {
		super(context);
		// slightly subtle: context here refers to the incoming context which still holds the parent
		parent = context.get(hxcoro.CoroTask.key);
		if (parent != null) {
			parent.addChild(this);
		}
	}

	function childSucceeds(_) {}

	function childErrors(_, error:Exception) {
		if (this.error == null) {
			this.error = error;
			cancel();
		}
	}

	function childCancels(_, cause:CancellationException) {}

	function complete() {
		parent?.childCompletes(this, false);
		handleAwaitingContinuations();
	}
}

class StartableCoroScopeTask<T> extends CoroScopeTask<T> implements IStartableCoroTask<T> {
	final lambda:NodeLambda<T>;

	/**
		Creates a new task using the provided `context` in order to execute `lambda`.
	**/
	public function new(context:Context, lambda:NodeLambda<T>) {
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
