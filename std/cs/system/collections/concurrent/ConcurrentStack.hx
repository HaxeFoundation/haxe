package cs.system.collections.concurrent;

@:native("System.Collections.Concurrent.ConcurrentStack")
extern class ConcurrentStack<T> {
	var Count(default, never):Int;
	var IsEmpty(default, never):Bool;
	@:overload(function():Void {})
	function new(collection:cs.system.collections.generic.IEnumerable<T>):Void;
	function Clear():Void;
	function CopyTo(array:cs.NativeArray<T>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
	function Push(item:T):Void;
	@:overload(function(items:cs.NativeArray<T>):Void {})
	function PushRange(items:cs.NativeArray<T>, startIndex:Int, count:Int):Void;
	function ToArray():cs.NativeArray<T>;
	function TryPeek(result:cs.Ref<T>):Bool;
	function TryPop(result:cs.Ref<T>):Bool;
	@:overload(function(items:cs.NativeArray<T>):Int {})
	function TryPopRange(items:cs.NativeArray<T>, startIndex:Int, count:Int):Int;
}
