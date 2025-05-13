package haxe.coro;

import haxe.CallStack.StackItem;
import haxe.Exception;

abstract class BaseContinuation<T> extends SuspensionResult<T> implements IContinuation<T> implements IStackFrame {
    public final completion:IContinuation<Any>;

	public final context:CoroutineContext;

    public var gotoLabel:Int;

    public var recursing:Bool;

    function new(completion:IContinuation<Any>, initialLabel:Int) {
        this.completion = completion;

        context    = completion.context;
        gotoLabel  = initialLabel;
        error      = null;
        result     = null;
        recursing  = false;
    }

    public final function resume(result:Any, error:Exception):Void {
        this.result = result;
        this.error  = error;
        context.scheduler.schedule(() -> {
			recursing = false;

			#if coroutine.throw
			try {
			#end
			final result = invokeResume();
			switch (result.state) {
				case Pending:
					return;
				case Returned:
					completion.resume(result.result, null);
				case Thrown:
					completion.resume(result.result, result.error);
			}
			#if coroutine.throw
			} catch (e:Dynamic) {
				completion.resume(null, @:privateAccess Exception.thrown(e));
			}
			#end
        });
    }

    public function callerFrame():Null<IStackFrame> {
        return if (completion is IStackFrame) {
            cast completion;
        } else {
            null;
        }
    }

	public function getStackItem():Null<StackItem> {
		return cast result;
	}

    public function setClassFuncStackItem(cls:String, func:String, file:String, line:Int, pos:Int, pmin:Int, pmax:Int) {
        result = cast StackItem.FilePos(StackItem.Method(cls, func), file, line, pos);
		#if eval
		eval.vm.Context.callMacroApi("associate_enum_value_pos")(result, haxe.macro.Context.makePosition({file: file, min: pmin, max: pmax}));
		#end
    }

    public function setLocalFuncStackItem(id:Int, file:String, line:Int, pos:Int, pmin:Int, pmax:Int) {
        result = cast StackItem.FilePos(StackItem.LocalFunction(id), file, line, pos);
		#if eval
		eval.vm.Context.callMacroApi("associate_enum_value_pos")(result, haxe.macro.Context.makePosition({file: file, min: pmin, max: pmax}));
		#end
    }

	public function startException(fromThrow:Bool) {
		if (fromThrow) {
			/*
				This comes from a coro-level throw, which pushes its position via one of the functions
				above. In this case we turn result into the stack item array now.
			*/
			result = cast [result];
		} else {
			/*
				This means we caught an exception, which must come from outside our current coro. We
				don't need our current result value because if anything it points to the last
				suspension call.
			*/
			result = cast [];
		}
	}

    public function buildCallStack() {
        var frame = callerFrame();
        if (frame != null) {
			var result:Array<StackItem> = cast result;
			result ??= [];
            result.push(frame.getStackItem());
        }
    }

    abstract function invokeResume():SuspensionResult<T>;
}