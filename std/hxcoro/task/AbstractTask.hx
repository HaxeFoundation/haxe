package hxcoro.task;

import hxcoro.concurrent.AtomicInt;
import haxe.coro.cancellation.ICancellationToken;
import haxe.coro.cancellation.ICancellationHandle;
import haxe.coro.cancellation.ICancellationCallback;
import haxe.exceptions.CancellationException;
import haxe.Exception;

enum abstract TaskState(Int) {
	final Created;
	final Running;
	final Completing;
	final Completed;
	final Cancelling;
	final Cancelled;
}

private class TaskException extends Exception {}

private class CancellationHandle implements ICancellationHandle {
	final callback:ICancellationCallback;
	final task:AbstractTask;

	var closed:Bool;

	public function new(callback, task) {
		this.callback = callback;
		this.task = task;

		closed = false;
	}

	public function run() {
		if (closed) {
			return;
		}

		final error = task.getError();
		callback.onCancellation(error.orCancellationException());

		closed = true;
	}

	public function close() {
		if (closed) {
			return;
		}
		final all = @:privateAccess task.cancellationCallbacks;

		if (all != null) {
			if (all.length == 1 && all[0] == this) {
				all.resize(0);
			} else {
				all.remove(this);
			}
		}

		closed = true;
	}
}

private class NoOpCancellationHandle implements ICancellationHandle {
	public function new() {}

	public function close() {}
}

/**
	AbstractTask is the base class for tasks which manages its `TaskState` and children.

	Developer note: it should have no knowledge of any asynchronous behavior or anything related to coroutines,
	and should be kept in a state where it could even be moved outside the hxcoro package. Also, `state` should
	be treated like a truly private variable and only be modified from within this class.
**/
abstract class AbstractTask<T = Any> implements ICancellationToken {
	static final atomicId = new AtomicInt(1); // start with 1 so we can use 0 for "no task" situations
	static final noOpCancellationHandle = new NoOpCancellationHandle();

	final parent:AbstractTask<Any>;

	var children:Null<Array<AbstractTask>>;
	var cancellationCallbacks:Null<Array<CancellationHandle>>;
	var state:TaskState;
	var error:Null<Exception>;
	var numCompletedChildren:Int;
	var indexInParent:Int;
	var allChildrenCompleted:Bool;

	public var id(get, null):Int;
	public var cancellationException(get, never):Null<CancellationException>;

	inline function get_cancellationException() {
		return switch state {
			case Cancelling | Cancelled:
				error.orCancellationException();
			case _:
				null;
		}
	}

	public inline function get_id() {
		return id;
	}

	/**
		Creates a new task.
	**/
	public function new(parent:Null<AbstractTask>, initialState:TaskState) {
		id = atomicId.add(1);
		this.parent = parent;
		state = Created;
		children = null;
		cancellationCallbacks = null;
		numCompletedChildren = 0;
		indexInParent = -1;
		allChildrenCompleted = false;
		if (parent != null) {
			parent.addChild(this);
		}
		switch (initialState) {
			case Created:
			case Running:
				start();
			case _:
				throw new TaskException('Invalid initial state $initialState');
		}
	}

	/**
		Returns the task's error value, if any/
	**/
	public function getError() {
		return error;
	}

	/**
		Initiates cancellation of this task and all its children.

		If `cause` is provided, it is set as this task's error value and used to cancel all children.

		If the task cannot be cancelled or has already been cancelled, this function only checks if the
		task has completed and initiates the appropriate behavior.
	**/
	public function cancel(?cause:CancellationException) {
		switch (state) {
			case Created | Running | Completing:
				cause ??= new CancellationException();
				if (error == null) {
					error = cause;
				}
				state = Cancelling;

				if (null != cancellationCallbacks) {
					for (h in cancellationCallbacks) {
						h.run();
					}
				}

				cancelChildren(cause);
				checkCompletion();
			case _:
				checkCompletion();
		}
	}

	/**
		Returns `true` if the task is still active. Note that an task that was created but not started yet
		is considered to be active.
	**/
	public function isActive() {
		return switch (state) {
			case Completed | Cancelled:
				false;
			case _:
				true;
		}
	}

	public function onCancellationRequested(callback:ICancellationCallback):ICancellationHandle {
		return switch state {
			case Cancelling | Cancelled:
				callback.onCancellation(error.orCancellationException());

				return noOpCancellationHandle;
			case _:
				final container = cancellationCallbacks ??= [];
				final handle = new CancellationHandle(callback, this);

				container.push(handle);

				handle;
		}
	}

	/**
		Returns this task's value, if any.
	**/
	abstract public function get():Null<T>;

	/**
		Starts executing this task. Has no effect if the task is already active or has completed.
	**/
	public function start() {
		switch (state) {
			case Created:
				state = Running;
				doStart();
			case _:
				return;
		}
	}

	public function cancelChildren(?cause:CancellationException) {
		if (null == children || children.length == 0) {
			return;
		}

		cause ??= new CancellationException();

		for (child in children) {
			if (child != null) {
				child.cancel(cause);
			}
		}
	}

	final inline function beginCompleting() {
		state = Completing;
		startChildren();
	}

	function startChildren() {
		if (null == children) {
			return;
		}

		for (child in children) {
			if (child == null) {
				continue;
			}
			switch (child.state) {
				case Created:
					child.start();
				case Cancelled | Completed:
				case Running | Completing | Cancelling:
			}
		}
	}

	function checkCompletion() {
		updateChildrenCompletion();
		if (!allChildrenCompleted) {
			return;
		}
		switch (state) {
			case Created | Running | Completed | Cancelled:
				return;
			case _:
		}
		switch (state) {
			case Completing:
				state = Completed;
			case Cancelling:
				state = Cancelled;
			case _:
				throw new TaskException('Invalid state $state in checkCompletion');
		}
		complete();
	}

	function updateChildrenCompletion() {
		if (allChildrenCompleted) {
			return;
		}
		if (children == null) {
			allChildrenCompleted = true;
			childrenCompleted();
		} else if (numCompletedChildren == children.length) {
			allChildrenCompleted = true;
			childrenCompleted();
		}
	}

	abstract function doStart():Void;

	abstract function complete():Void;

	abstract function childrenCompleted():Void;

	abstract function childSucceeds(child:AbstractTask):Void;

	abstract function childErrors(child:AbstractTask, cause:Exception):Void;

	abstract function childCancels(child:AbstractTask, cause:CancellationException):Void;

	// called from child

	function childCompletes(child:AbstractTask, processResult:Bool) {
		numCompletedChildren++;
		if (processResult) {
			if (child.error != null) {
				if (child.error is CancellationException) {
					childCancels(child, cast child.error);
				} else {
					childErrors(child, child.error);
				}
			} else {
				childSucceeds(child);
			}
		}
		updateChildrenCompletion();
		checkCompletion();
		if (child.indexInParent >= 0) {
			children[child.indexInParent] = null;
		}
	}

	function addChild(child:AbstractTask) {
		final container = children ??= [];
		final index = container.push(child);
		child.indexInParent = index - 1;
	}
}
