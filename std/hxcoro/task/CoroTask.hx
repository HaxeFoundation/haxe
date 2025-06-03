package hxcoro.task;

import hxcoro.task.ICoroTask;
import hxcoro.task.AbstractTask;
import hxcoro.task.ICoroNode;
import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.context.Key;
import haxe.coro.context.Context;
import haxe.coro.context.IElement;
import haxe.coro.schedulers.Scheduler;
import haxe.coro.cancellation.CancellationToken;

private class CoroTaskWith<T> implements ICoroNodeWith {
	public var context(get, null):Context;

	final task:CoroTask<T>;

	public function new(context:Context, task:CoroTask<T>) {
		this.context = context;
		this.task = task;
	}

	inline function get_context() {
		return context;
	}

	public function async<T>(lambda:NodeLambda<T>):ICoroTask<T> {
		final child = new CoroChildTask(context, task);
		context.get(Scheduler.key).schedule(0, () -> {
			child.runNodeLambda(lambda);
		});
		return child;
	}

	public function lazy<T>(lambda:NodeLambda<T>):IStartableCoroTask<T> {
		return new CoroChildTask.StartableCoroChildTask(context, lambda, task);
	}

	public function with(...elements:IElement<Any>) {
		return task.with(...elements);
	}
}

/**
	CoroTask provides the basic functionality for coroutine tasks.
**/
abstract class CoroTask<T> extends AbstractTask<T> implements IContinuation<T> implements ICoroNode implements ICoroTask<T> implements IElement<CoroTask<Any>> {
	public static final key:Key<CoroTask<Any>> = Key.createNew('Task');

	/**
		This task's immutable `Context`.
	**/
	public var context(get, null):Context;

	var initialContext:Context;
	var result:Null<T>;
	var awaitingContinuations:Null<Array<IContinuation<T>>>;
	var awaitingChildContinuation:Null<IContinuation<Any>>;
	var wasResumed:Bool;

	/**
		Creates a new task using the provided `context`.
	**/
	public function new(context:Context) {
		super();
		initialContext = context;
		wasResumed = true;
	}

	inline function get_context() {
		if (context == null) {
			context = initialContext.clone().with(this).add(CancellationToken.key, this);
		}
		return context;
	}

	public function get() {
		return result;
	}

	public function getKey() {
		return key;
	}

	public function doStart() {
		wasResumed = false;
	}

	/**
		Indicates that the task has been suspended, which allows it to clean up some of
		its internal resources. Has no effect on the observable state of the task.

		This function should be called when it is expected that the task might not be resumed
		for a while, e.g. when waiting on a sparse `Channel` or a contended `Mutex`.
	**/
	public function putOnHold() {
		context = null;
		if (awaitingContinuations != null && awaitingContinuations.length == 0) {
			awaitingContinuations = null;
		}
		if (cancellationCallbacks != null && cancellationCallbacks.length == 0) {
			cancellationCallbacks = null;
		}
		if (allChildrenCompleted) {
			children = null;
		}
	}

	public function runNodeLambda(lambda:NodeLambda<T>) {
		final result = lambda(this, this);
		start();
		switch result.state {
			case Pending:
				return;
			case Returned:
				resume(result.result, null);
			case Thrown:
				resume(null, result.error);
		}
	}

	/**
		Creates a lazy child task to execute `lambda`. The child task does not execute until its `start`
		method is called. This occurrs automatically once this task has finished execution.
	**/
	public function lazy<T>(lambda:NodeLambda<T>):IStartableCoroTask<T> {
		return new CoroChildTask.StartableCoroChildTask(initialContext, lambda, this);
	}

	/**
		Creates a child task to execute `lambda` and starts it automatically.
	**/
	public function async<T>(lambda:NodeLambda<T>):ICoroTask<T> {
		final child = new CoroChildTask<T>(initialContext, this);
		initialContext.get(Scheduler.key).schedule(0, () -> {
			child.runNodeLambda(lambda);
		});
		return child;
	}

	/**
		Returns a copy of this tasks `Context` with `elements` added, which can be used to start child tasks.
	**/
	public function with(...elements:IElement<Any>) {
		return new CoroTaskWith(context.clone().with(...elements), this);
	}

	/**
		Resumes `cont` with this task's outcome.

		If this task is no longer active, the continuation is resumed immediately. Otherwise, it is registered
		to be resumed upon completion.

		This function also starts this task if it has not been started yet.
	**/
	public function awaitContinuation(cont:IContinuation<T>) {
		switch state {
			case Completed:
				cont.resume(result, null);
			case Cancelled:
				cont.resume(null, error);
			case _:
				awaitingContinuations ??= [];
				awaitingContinuations.push(cont);
				start();
		}
	}

	@:coroutine public function awaitChildren() {
		if (allChildrenCompleted) {
			awaitingChildContinuation?.resume(null, null);
		}
		startChildren();
		Coro.suspend(cont -> awaitingChildContinuation = cont);
	}

	/**
		Suspends this task until it completes.
	**/
	@:coroutine public function await():T {
		return Coro.suspend(awaitContinuation);
	}

	/**
		Resumes the task with the provided `result` and `error`.
	**/
	public function resume(result:T, error:Exception) {
		wasResumed = true;
		if (error == null) {
			switch (state) {
				case Running:
					this.result = result;
					beginCompleting();
				case _:
			}
			checkCompletion();
		} else {
			if (this.error == null) {
				this.error = error;
			}
			cancel();
		}
	}

	override function checkCompletion() {
		if (!wasResumed) {
			return;
		}
		super.checkCompletion();
	}

	function childrenCompleted() {
		awaitingChildContinuation?.resume(null, null);
	}

	function handleAwaitingContinuations() {
		if (awaitingContinuations == null) {
			return;
		}
		while (awaitingContinuations.length > 0) {
			final continuations = awaitingContinuations;
			awaitingContinuations = [];
			if (error != null) {
				for (cont in continuations) {
					cont.resume(null, error);
				}
			} else {
				for (cont in continuations) {
					cont.resume(result, null);
				}
			}
		}
	}
}
