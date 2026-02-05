package cs.system.collections.concurrent;

@:native("System.Collections.Concurrent.ConcurrentQueue")
extern class ConcurrentQueue<T> {
	var Count(default, never):Int;
	var IsEmpty(default, never):Bool;
	@:overload(function():Void {})
	function new(collection:cs.system.collections.generic.IEnumerable<T>):Void;
	function Clear():Void;
	function CopyTo(array:cs.NativeArray<T>, index:Int):Void;
	function Enqueue(item:T):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
	function ToArray():cs.NativeArray<T>;
	function TryDequeue(result:cs.Ref<T>):Bool;
	function TryPeek(result:cs.Ref<T>):Bool;
}
