package hxcoro;

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

class TaskException extends Exception {}

/**
	AbstractTask is the base class for tasks which manages its `TaskState` and children.

	Developer note: it should have no knowledge of any asynchronous behavior or anything related to coroutines,
	and should be kept in a state where it could even be moved outside the hxcoro package. Also, `state` should
	be treated like a truly private variable and only be modified from within this class.
**/
abstract class AbstractTask<T> {
	final children:Array<AbstractTask<Any>>;
	var state:TaskState;
	var error:Null<Exception>;
	var numCompletedChildren:Int;
	var indexInParent:Int;

	/**
		Creates a new task.
	**/
	public function new() {
		state = Created;
		children = [];
		numCompletedChildren = 0;
		indexInParent = -1;
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

	/**
		Returns `true` if cancellation has been requested. This remains true even if cancellation has completed.
	**/
	public function cancellationRequested() {
		return switch (state) {
			case Cancelling | Cancelled:
				true;
			case _:
				false;
		}
	}

	/**
		Returns this task's value, if any.
	**/
	abstract public function get():Null<T>;

	/**
		Starts executing this task. Has no effect if the task is already active or has completed.
	**/
	abstract public function start():Void;

	function cancelChildren(?cause:CancellationException) {
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

	final inline function beginRunning() {
		state = Running;
	}

	function startChildren() {
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
		switch (state) {
			case Created | Running | Completed | Cancelled:
				return;
			case _:
		}
		if (numCompletedChildren != children.length) {
			return;
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

	abstract function complete():Void;

	abstract function childSucceeds(child:AbstractTask<Any>):Void;

	abstract function childErrors(child:AbstractTask<Any>, cause:Exception):Void;

	abstract function childCancels(child:AbstractTask<Any>, cause:CancellationException):Void;

	// called from child

	function childCompletes(child:AbstractTask<Any>, processResult:Bool) {
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
		checkCompletion();
		if (child.indexInParent >= 0) {
			children[child.indexInParent] = null;
		}
	}

	function addChild(child:AbstractTask<Any>) {
		final index = children.push(child);
		child.indexInParent = index - 1;
	}
}
