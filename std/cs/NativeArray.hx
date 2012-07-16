package cs;

extern class NativeArray<T> extends cs.system.Array, implements ArrayAccess<T>
{
	public var Length(default, null):Int;
	
	public function new(len:Int):Void;
	
	@:overload(function(arr:cs.system.Array, destIndex:haxe.Int64):Void {} )
	public function CopyTo(arr:cs.system.Array, destIndex:Int):Void;
	
	static function Reverse(arr:cs.system.Array):Void;
}