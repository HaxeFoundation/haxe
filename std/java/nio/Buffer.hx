package java.nio;

extern class Buffer 
{
	function capacity():Int;
	function clear():Buffer;
	function flip():Buffer;
	function hasRemaining():Bool;
	function isReadOnly():Bool;
	@:overload(function(newLimit:Int):Buffer {})
	function limit():Int;
	function mark():Buffer;
	@:overload(function(newPosition:Int):Buffer { })
	function position():Int;
	function remaining():Int;
	function reset():Buffer;
	function rewind():Buffer;
}