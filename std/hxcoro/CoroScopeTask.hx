package hxcoro;

import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.coro.context.Context;

class CoroScopeTask<T> extends CoroTask<T> {
	final parent:Null<AbstractTask<Any>>;

	public function new(context:Context, lambda:NodeLambda<T>) {
		super(context, lambda);
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
