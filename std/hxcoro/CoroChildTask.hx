package hxcoro;

import haxe.coro.context.Context;
import haxe.Exception;
import haxe.exceptions.CancellationException;

class CoroChildTask<T> extends CoroTask<T> {
	final parent:AbstractTask<Any>;

	public function new(context:Context, lambda:NodeLambda<T>, parent:AbstractTask<Any>) {
		super(context, lambda);
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
