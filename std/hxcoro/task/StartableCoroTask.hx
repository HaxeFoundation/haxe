package hxcoro.task;

import hxcoro.task.node.INodeStrategy;
import hxcoro.task.ICoroTask;
import haxe.coro.context.Context;

class StartableCoroTask<T> extends CoroTask<T> implements IStartableCoroTask<T> {
	final lambda:NodeLambda<T>;

	/**
		Creates a new task using the provided `context` in order to execute `lambda`.
	**/
	public function new(context:Context, lambda:NodeLambda<T>, nodeStrategy:INodeStrategy) {
		super(context, nodeStrategy, Created);
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
