package cs.system.collections.generic;

@:native("System.Collections.Generic.Queue")
extern class Queue<T> {
	var Count(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<T>):Void {})
	function new(capacity:Int):Void;
	function Clear():Void;
	function Contains(item:T):Bool;
	function CopyTo(array:cs.NativeArray<T>, arrayIndex:Int):Void;
	function Dequeue():T;
	function Enqueue(item:T):Void;
	function GetEnumerator():cs.system.collections.generic.Queue_Enumerator<T>;
	function Peek():T;
	function ToArray():cs.NativeArray<T>;
	function TrimExcess():Void;
	function TryDequeue(result:cs.Ref<T>):Bool;
	function TryPeek(result:cs.Ref<T>):Bool;
}
