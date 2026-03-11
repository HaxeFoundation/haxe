package haxe.coro.context;

import haxe.Exception;
import haxe.PosInfos;
import haxe.coro.context.Context;
import haxe.coro.context.IElement;
import haxe.coro.context.Key;

using StringTools;

interface ISynchronousRun {
	public var context(get, never):Context;
	function complete():Void;
}
/**
	An abstract context element that handles exception stack trace management for coroutines.
	`BaseContinuation.startException` and `BaseContinuation.buildCallStack` delegate to this element.

	The default implementation is `DefaultExceptionHandler`.
**/
abstract class ExceptionHandler implements IElement<ExceptionHandler> {
	public static final key = new Key<ExceptionHandler>('ExceptionHandler');

	abstract public function startSynchronousRun(context:Context, p:PosInfos):ISynchronousRun;

	/**
		Called when an exception is first encountered in a coroutine to process its stack trace.
		Returns the (potentially modified) exception.
	**/
	abstract public function startException(context:Context, frame:IStackFrame, exception:Exception):Exception;

	/**
		Called as an exception propagates up the coroutine continuation chain, to insert each
		continuation frame's stack item into the exception stack.
	**/
	abstract public function buildCallStack(context:Context, frame:IStackFrame):Void;

	public function getKey() {
		return key;
	}
}