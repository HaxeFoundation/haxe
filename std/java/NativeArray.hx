package java;

/**
 * ...
 * @author waneck
 */

@:nativegen extern class NativeArray<T> implements ArrayAccess<T>
{
	public var length(default, null):Int;
	
	public function new(len:Int):Void;
	
}