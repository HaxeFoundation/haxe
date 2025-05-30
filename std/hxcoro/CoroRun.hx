package hxcoro;

import haxe.coro.Coroutine;
import haxe.coro.context.Context;
import haxe.coro.context.IElement;
import haxe.coro.schedulers.EventLoopScheduler;
import hxcoro.ICoroTask;

private abstract RunnableContext(ElementTree) {
	inline function new(tree:ElementTree) {
		this = tree;
	}

	public function create<T>(lambda:NodeLambda<T>):IStartableCoroTask<T> {
		return new CoroScopeTask(new Context(this), lambda);
	}

	public function run<T>(lambda:NodeLambda<T>):T {
		return CoroRun.runWith(new Context(this), lambda);
	}

	@:from static function fromAdjustableContext(context:AdjustableContext) {
		return new RunnableContext(cast context);
	}

	public function with(...elements:IElement<Any>):RunnableContext {
		return new AdjustableContext(this.copy()).with(...elements);
	}
}

class CoroRun {
	static var defaultContext(get, null):Context;

	static function get_defaultContext() {
		if (defaultContext != null) {
			return defaultContext;
		}
		final stackTraceManagerComponent = new haxe.coro.BaseContinuation.StackTraceManager();
		defaultContext = Context.create(stackTraceManagerComponent);
		return defaultContext;
	}

	public static function with(...elements:IElement<Any>):RunnableContext {
		return defaultContext.clone().with(...elements);
	}

	static public function run<T>(lambda:Coroutine<() -> T>):T {
		return runScoped(_ -> lambda());
	}

	static public function runScoped<T>(lambda:NodeLambda<T>):T {
		return runWith(defaultContext, lambda);
	}

	static public function runWith<T>(context:Context, lambda:NodeLambda<T>):T {
		final schedulerComponent = new EventLoopScheduler();
		final scope = new CoroScopeTask(context.clone().with(schedulerComponent), lambda);
		scope.start();
		while (scope.isActive()) {
			schedulerComponent.run();
		}
		switch (scope.getError()) {
			case null:
				return scope.get();
			case error:
				throw error;
		}
	}
}