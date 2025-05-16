package haxe.coro;

import haxe.coro.context.Context;
import haxe.coro.schedulers.Scheduler;
import haxe.CallStack.StackItem;
import haxe.Exception;

private enum abstract ExceptionMode(Int) {
	/**
		The exception was raised by our own coroutine.
	**/
	var ExceptionSelf;
	/**
		The exception was raised further up the call stack, e.g. from a function
		our current coroutine called.
	**/
	var ExceptionTop;
	/**
		The exception was created (but not raised) by a suspension function further
		up the call stack and returned to our current coroutine.
	**/
	var ExceptionImmediate;
}

abstract class BaseContinuation<T> extends SuspensionResult<T> implements IContinuation<T> implements IStackFrame {
    public final completion:IContinuation<Any>;

	public final context:Context;

    public var gotoLabel:Int;

    public var recursing:Bool;

	var callStackOnFirstSuspension:Null<Array<StackItem>>;
	var startedException:Bool;

    function new(completion:IContinuation<Any>, initialLabel:Int) {
        this.completion = completion;

        context    = completion.context;
        gotoLabel  = initialLabel;
        error      = null;
        result     = null;
        recursing  = false;
		startedException = false;
    }

    public final function resume(result:Any, error:Exception):Void {
        this.result = result;
        this.error  = error;
        context.get(Scheduler.key).schedule(() -> {
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
		callStackOnFirstSuspension ??= CallStack.callStack();
		#if eval
		eval.vm.Context.callMacroApi("associate_enum_value_pos")(result, haxe.macro.Context.makePosition({file: file, min: pmin, max: pmax}));
		#end
    }

    public function setLocalFuncStackItem(id:Int, file:String, line:Int, pos:Int, pmin:Int, pmax:Int) {
        result = cast StackItem.FilePos(StackItem.LocalFunction(id), file, line, pos);
		callStackOnFirstSuspension ??= CallStack.callStack();
		#if eval
		eval.vm.Context.callMacroApi("associate_enum_value_pos")(result, haxe.macro.Context.makePosition({file: file, min: pmin, max: pmax}));
		#end
    }

	public function startException(exceptionMode:ExceptionMode) {
		startedException = true;
		if (callStackOnFirstSuspension != null) {
			/*
				On the first suspension of any coroutine we record the synchronous call stack which tells
				us how we got here. This will ensure we don't miss synchronous stack items, such as ones
				from a TCOed parent function.

				We skip the two topmost elements because they're from the set functions above and the call
				to them from the state machine.
			*/
			callStackOnFirstSuspension = CallStackHelper.cullTopStack(callStackOnFirstSuspension, 2);
		} else {
			/**
				This can only occur in ExceptionTop mode and means we caught a foreigh exception.
			**/
			result = cast callStackOnFirstSuspension = [];
			return;
		}
		switch (exceptionMode) {
			case ExceptionSelf | ExceptionImmediate:
				/*
					In these modes we add our current stack item as the topmost element to the call-stack.
					In both cases the value will be set:
						* A `throw` in Self mode is always preceeded by a call to one of the set functions above.
						* Immediate mode only occurs after a suspension call, which also calls a set function.
				*/
				callStackOnFirstSuspension.unshift(cast result);
			case ExceptionTop:
		}
		result = cast callStackOnFirstSuspension;
	}

    public function buildCallStack() {
		if (startedException) {
			/*
				If we started the exception in our current coroutine then we don't need to do any additional
				management. The caller frame will be part of the top stack added by startException.
			*/
			return;
		}
        var frame = callerFrame();
        if (frame != null) {
			var result:Array<StackItem> = cast result;
			result ??= [];
            result.push(frame.getStackItem());
        }
    }

    abstract function invokeResume():SuspensionResult<T>;
}