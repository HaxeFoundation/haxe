package cs;

extern class NativeArray<T> extends system.Array, implements ArrayAccess<T>
{
	public var Length(default, null):Int;
	
	public function new(len:Int):Void;
	
	@:overload(function(arr:system.Array, destIndex:Int64):Void {} )
	public function CopyTo(arr:system.Array, destIndex:Int):Void;
	
	
}