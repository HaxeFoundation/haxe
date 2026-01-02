package haxe.coro;

import haxe.CallStack.StackItem;

/**
	This interface is internally used to manage coroutine call stacks. Only meaningful in `-debug` mode.
**/
interface IStackFrame {
	/**
		Returns the current `StackItem`, which is generally the location this frame was
		called from. Can be `null` if no such information exists.
	**/
    function getStackItem():Null<StackItem>;
	/**
		Returns the frame of the caller, or `null` if there isn't any.
	**/
	function callerFrame():Null<IStackFrame>;
}