package cs.system.collections.concurrent;

@:native("System.Collections.Concurrent.ConcurrentBag")
extern class ConcurrentBag<T> {
	var Count(default, never):Int;
	var IsEmpty(default, never):Bool;
	@:overload(function():Void {})
	function new(collection:cs.system.collections.generic.IEnumerable<T>):Void;
	function Add(item:T):Void;
	function Clear():Void;
	function CopyTo(array:cs.NativeArray<T>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
	function ToArray():cs.NativeArray<T>;
	function TryPeek(result:cs.Ref<T>):Bool;
	function TryTake(result:cs.Ref<T>):Bool;
}
