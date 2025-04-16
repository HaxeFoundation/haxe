package haxe.coro;

import haxe.Exception;

abstract class BaseContinuation extends ContinuationResult implements IContinuation<Any> {
    public final _hx_completion:IContinuation<Any>;

	public final _hx_context:CoroutineContext;

    public var _hx_state:Int;

    public var _hx_recursing:Bool;

    function new(completion:IContinuation<Any>, initialState:Int) {
        _hx_completion = completion;
        _hx_context    = completion._hx_context;
        _hx_state      = initialState;
        _hx_error      = null;
        _hx_result     = null;
        _hx_recursing  = false;
    }

    public final function resume(result:Any, error:Exception):Void {
        _hx_result = result;
        _hx_error  = error;
        _hx_context.scheduler.schedule(() -> {
            try
            {
                _hx_recursing = false;

                final result = invokeResume();
                if (result is Primitive) {
                    return;
                }

                _hx_completion.resume(result, null);
            }
            catch (exn:Exception) {
                _hx_completion.resume(null, exn);
            }
        });
    }

    abstract function invokeResume():Any;
}