package haxe.coro;

import haxe.CallStack.StackItem;

interface IStackFrame {
    function getStackItem():Null<StackItem>;
	function callerFrame():Null<IStackFrame>;
}