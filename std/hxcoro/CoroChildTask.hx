package hxcoro;

import haxe.coro.context.Context;
import haxe.Exception;
import haxe.exceptions.CancellationException;
import hxcoro.ICoroTask;

class CoroChildTask<T> extends CoroTask<T> {
	final parent:AbstractTask<Any>;

	public function new(context:Context, parent:AbstractTask<Any>) {
		super(context);
		this.parent = parent;
		parent.addChild(this);
	}

	// called from parent

	function childSucceeds(child:AbstractTask<Any>) {}

	function childErrors(child:AbstractTask<Any>, error:Exception) {
		switch (state) {
			case Created | Running | Completing:
				// inherit child error
				if (this.error == null) {
					this.error = error;
				}
				cancel();
			case Cancelling:
				// not sure about this one, what if we cancel normally and then get a real exception?
			case Completed | Cancelled:
		}
	}

	function childCancels(child:AbstractTask<Any>, cause:CancellationException) {
		// Cancellation is often issued from the parent anyway, but I don't know if that's always the case
		// Calling cancel is fine because it won't do anything if we're already cancelling
		cancel(cause);
	}

	function complete() {
		parent?.childCompletes(this, true);
		handleAwaitingContinuations();
	}
}

class StartableCoroChildTask<T> extends CoroChildTask<T> implements IStartableCoroTask<T> {
	final lambda:NodeLambda<T>;

	/**
		Creates a new task using the provided `context` in order to execute `lambda`.
	**/
	public function new(context:Context, lambda:NodeLambda<T>, parent:AbstractTask<Any>) {
		super(context, parent);
		this.lambda = lambda;
	}

	/**
		Starts executing this task's `lambda`. Has no effect if the task is already active or has completed.
	**/
	override public function doStart() {
		super.doStart();
		runNodeLambda(lambda);
	}
}
