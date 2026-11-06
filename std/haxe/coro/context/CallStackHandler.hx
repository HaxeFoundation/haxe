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
	A context element that builds the call stacks of exceptions thrown in coroutines.
**/
abstract class CallStackHandler implements IElement<CallStackHandler> {
	public static final key = new Key<CallStackHandler>('CallStackHandler');

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