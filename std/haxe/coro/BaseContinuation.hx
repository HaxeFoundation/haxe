package haxe.coro;

import haxe.CallStack.StackItem;
import haxe.Exception;

abstract class BaseContinuation<T> extends SuspensionResult<T> implements IContinuation<T> implements IStackFrame {
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
			_hx_recursing = false;

			#if coroutine.throw
			try {
			#end
			final result = invokeResume();
			switch (result._hx_control) {
				case Pending:
					return;
				case Returned:
					_hx_completion.resume(result._hx_result, null);
				case Thrown:
					_hx_completion.resume(result._hx_result, result._hx_error);
			}
			#if coroutine.throw
			} catch (e:Dynamic) {
				_hx_completion.resume(null, @:privateAccess Exception.thrown(e));
			}
			#end
        });
    }

    public function callerFrame():Null<IStackFrame> {
        return if (_hx_completion is IStackFrame) {
            cast _hx_completion;
        } else {
            null;
        }
    }

	public function getStackItem():Null<StackItem> {
		return cast _hx_result;
	}

    public function setClassFuncStackItem(cls:String, func:String, file:String, line:Int, pos:Int, pmin:Int, pmax:Int) {
        _hx_result = cast StackItem.FilePos(StackItem.Method(cls, func), file, line, pos);
    }

    public function setLocalFuncStackItem(id:Int, file:String, line:Int, pos:Int, pmin:Int, pmax:Int) {
        _hx_result = cast StackItem.FilePos(StackItem.LocalFunction(id), file, line, pos);
    }

	public function startException(fromThrow:Bool) {
		if (fromThrow) {
			/*
				This comes from a coro-level throw, which pushes its position via one of the functions
				above. In this case we turn _hx_result into the stack item array now.
			*/
			_hx_result = cast [_hx_result];
		} else {
			/*
				This means we caught an exception, which must come from outside our current coro. We
				don't need our current _hx_result value because if anything it points to the last
				suspension call.
			*/
			_hx_result = cast [];
		}
	}

    public function buildCallStack() {
        var frame = callerFrame();
        if (frame != null) {
            (cast _hx_result : Array<StackItem>).push(frame.getStackItem());
        }
    }

    abstract function invokeResume():SuspensionResult<T>;
}