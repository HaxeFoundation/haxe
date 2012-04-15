package cs;

/**
 * ...
 * @author waneck
 */

extern class NativeArray<T> extends cs.native.Array, implements ArrayAccess<T>
{
	public var Length(default, null):Int;
	
	public function new(len:Int):Void;
	
	@:overload(function(arr:cs.native.Array, destIndex:Int64):Void {} )
	public function CopyTo(arr:cs.native.Array, destIndex:Int):Void;
	
	
}