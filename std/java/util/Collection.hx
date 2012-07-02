package java.util;

extern interface Collection<E> implements java.util.Iterable<E>
{
	function add(o:E):Bool;
	function addAll(c:Collection<E>):Bool;
	function clear():Void;
	function contains(o:Dynamic):Bool;
	function containsAll(c:Collection<Dynamic>):Bool;
	function isEmpty():Bool;
	function iterator():Iterator<E>;
	function remove(o:Dynamic):Bool;
	function size():Int;
	function toArray():java.NativeArray<Dynamic>;
}