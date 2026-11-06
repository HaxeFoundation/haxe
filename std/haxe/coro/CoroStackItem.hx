package haxe.coro;

import haxe.PosInfos;

/**
	Internal coroutine stack frame item, translated to `haxe.CallStack.StackItem`
	by `haxe.coro.context.CallStackHandler`.
**/
enum CoroStackItem {
	ClassFunction(cls:String, func:String, file:String, line:Int, column:Int);
	LocalFunction(id:Int, file:String, line:Int, column:Int);
	PosInfo(p:PosInfos);
}
