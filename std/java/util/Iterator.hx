package java.util;

extern interface Iterator<T>
{
	function hasNext():Bool;
	function next():T;
	function remove():Void;
}