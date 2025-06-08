package hxcoro.task;

import hxcoro.task.CoroTask;
import hxcoro.task.node.INodeStrategy;
import hxcoro.task.ICoroTask;
import hxcoro.task.AbstractTask;
import hxcoro.task.ICoroNode;
import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.coro.IContinuation;
import haxe.coro.context.Context;
import haxe.coro.context.Key;
import haxe.coro.context.IElement;
import haxe.coro.schedulers.Scheduler;
import haxe.coro.cancellation.CancellationToken;

private class CoroTaskWith<T> implements ICoroNodeWith {
	public var context(get, null):Context;

	final task:CoroBaseTask<T>;

	public function new(context:Context, task:CoroBaseTask<T>) {
		this.context = context;
		this.task = task;
	}

	inline function get_context() {
		return context;
	}

	public function async<T>(lambda:NodeLambda<T>):ICoroTask<T> {
		final child = new CoroTaskWithLambda(context, lambda, CoroTask.CoroChildStrategy);
		context.get(Scheduler).scheduleObject(child);
		return child;
	}

	public function lazy<T>(lambda:NodeLambda<T>):IStartableCoroTask<T> {
		return new StartableCoroTask(context, lambda, CoroTask.CoroChildStrategy);
	}

	public function with(...elements:IElement<Any>) {
		return task.with(...elements);
	}
}

private class CoroKeys {
	static public final awaitingChildContinuation = new Key<IContinuation<Any>>("AwaitingChildContinuation");
}

/**
	CoroTask provides the basic functionality for coroutine tasks.
**/
abstract class CoroBaseTask<T> extends AbstractTask<T> implements ICoroNode implements ICoroTask<T> implements ILocalContext implements IElement<CoroBaseTask<Any>> {
	/**
		This task's immutable `Context`.
	**/
	public var context(get, null):Context;

	final nodeStrategy:INodeStrategy;
	var coroLocalContext:Null<AdjustableContext>;
	var initialContext:Context;
	var result:Null<T>;
	var awaitingContinuations:Null<Array<IContinuation<T>>>;

	/**
		Creates a new task using the provided `context`.
	**/
	public function new(context:Context, nodeStrategy:INodeStrategy, initialState:TaskState) {
		super(context.get(CoroTask), initialState);
		initialContext = context;
		this.nodeStrategy = nodeStrategy;
	}

	inline function get_context() {
		if (context == null) {
			context = initialContext.clone().with(this).add(CancellationToken, this);
		}
		return context;
	}

	public function get() {
		return result;
	}

	public function getKey() {
		return CoroTask.key;
	}

	public function getLocalElement<T>(key:Key<T>):Null<T> {
		return coroLocalContext?.get(key);
	}

	public function setLocalElement<T>(key:Key<T>, element:T) {
		if (coroLocalContext == null) {
			coroLocalContext = Context.create();
		}
		coroLocalContext.add(key, element);
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

	/**
		Creates a lazy child task to execute `lambda`. The child task does not execute until its `start`
		method is called. This occurrs automatically once this task has finished execution.
	**/
	public function lazy<T>(lambda:NodeLambda<T>):IStartableCoroTask<T> {
		return new StartableCoroTask(context, lambda, CoroTask.CoroChildStrategy);
	}

	/**
		Creates a child task to execute `lambda` and starts it automatically.
	**/
	public function async<T>(lambda:NodeLambda<T>):ICoroTask<T> {
		final child = new CoroTaskWithLambda<T>(context, lambda, CoroTask.CoroChildStrategy);
		context.get(Scheduler).scheduleObject(child);
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
				cont.succeedSync(result);
			case Cancelled:
				cont.failSync(error);
			case _:
				awaitingContinuations ??= [];
				awaitingContinuations.push(cont);
				start();
		}
	}

	@:coroutine public function awaitChildren() {
		if (allChildrenCompleted) {
			getLocalElement(CoroKeys.awaitingChildContinuation)?.callSync();
			return;
		}
		startChildren();
		Coro.suspend(cont -> setLocalElement(CoroKeys.awaitingChildContinuation, cont));
	}

	/**
		Suspends this task until it completes.
	**/
	@:coroutine public function await():T {
		return Coro.suspend(awaitContinuation);
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
					cont.failAsync(error);
				}
			} else {
				for (cont in continuations) {
					cont.succeedAsync(result);
				}
			}
		}
	}

	function childrenCompleted() {
		getLocalElement(CoroKeys.awaitingChildContinuation)?.callSync();
	}

	// strategy dispatcher

	function complete() {
		nodeStrategy.complete(this);
	}

	function childSucceeds(child:AbstractTask) {
		nodeStrategy.childSucceeds(this, child);
	}

	function childErrors(child:AbstractTask, cause:Exception) {
		nodeStrategy.childErrors(this, child, cause);
	}

	function childCancels(child:AbstractTask, cause:CancellationException) {
		nodeStrategy.childCancels(this, child, cause);
	}
}
