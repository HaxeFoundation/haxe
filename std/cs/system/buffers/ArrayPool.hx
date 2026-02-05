package cs.system.buffers;

@:native("System.Buffers.ArrayPool")
extern class ArrayPool<T> {
	static var Shared(default, never):cs.system.buffers.ArrayPool<Dynamic>;
	@:overload(function<T>():cs.system.buffers.ArrayPool<T> {})
	static function Create<T>(maxArrayLength:Int, maxArraysPerBucket:Int):cs.system.buffers.ArrayPool<T>;
	function Rent(minimumLength:Int):cs.NativeArray<T>;
	function Return(array:cs.NativeArray<T>, ?clearArray:Bool):Void;
}
