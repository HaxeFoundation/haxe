package hxcoro.task.node;

import haxe.Exception;
import haxe.exceptions.CancellationException;

@:access(hxcoro.task.AbstractTask)
@:access(hxcoro.task.CoroTask)
class CoroChildStrategy implements INodeStrategy {
	public function new() {}

	public function complete<T>(task:CoroBaseTask<T>) {
		task.parent?.childCompletes(task, true);
		task.handleAwaitingContinuations();
	}

	public function childSucceeds<T>(task:CoroBaseTask<T>, child:AbstractTask) {}

	public function childErrors<T>(task:CoroBaseTask<T>, child:AbstractTask, cause:Exception) {
		switch (task.state) {
			case Created | Running | Completing:
				// inherit child error
				if (task.error == null) {
					task.error = cause;
				}
				task.cancel();
			case Cancelling:
				// not sure about this one, what if we cancel normally and then get a real exception?
			case Completed | Cancelled:
		}
	}

	public function childCancels<T>(task:CoroBaseTask<T>, child:AbstractTask, cause:CancellationException) {
		task.cancel(cause);
	}
}
