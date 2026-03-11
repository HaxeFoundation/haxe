package haxe.coro;

/**
	This interface is internally used to manage coroutine call stacks. Only meaningful in `-debug` mode.
**/
interface IStackFrame {
	/**
		Returns the current `CoroStackItem`, which is generally the location this frame was
		called from. Can be `null` if no such information exists.
	**/
	function getStackItem():Null<CoroStackItem>;
	/**
		Returns the frame of the caller, or `null` if there isn't any.
	**/
	function callerFrame():Null<IStackFrame>;
}
