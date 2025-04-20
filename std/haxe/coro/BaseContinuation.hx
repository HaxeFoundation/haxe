package haxe.coro;

import haxe.Exception;

abstract class BaseContinuation<T> extends ContinuationResult<T> implements IContinuation<T> {
	public final _hx_completion:IContinuation<Any>;

	public final _hx_context:CoroutineContext;

	public var _hx_state:Int;

	public var _hx_recursing:Bool;

	public final name:String;

	function new(completion:IContinuation<Any>, initialState:Int) {
		_hx_completion = completion;
		_hx_context    = completion._hx_context;
		_hx_state      = initialState;
		_hx_error      = null;
		_hx_result     = null;
		_hx_recursing  = false;
		name           = Type.getClassName(Type.getClass(this));
	}

	public final function resume(result:Any, error:Exception):Void {
		_hx_result = result;
		_hx_error  = error;
		_hx_context.scheduler.schedule(() -> {
			_hx_recursing = false;

			final result = invokeResume();
			switch (result._hx_control) {
				case Pending:
					return;
				case Returned:
					_hx_context.maybeIntercept(_hx_completion, result._hx_result, null);
				case Thrown:
					_hx_context.maybeIntercept(_hx_completion, null, result._hx_error);
			}
		});
	}

	abstract function invokeResume():ContinuationResult<T>;

	override public function toString() {
		return '[$name state $_hx_state, ${super.toString()}]';
	}
}