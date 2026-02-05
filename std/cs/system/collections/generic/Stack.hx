package cs.system.collections.generic;

@:native("System.Collections.Generic.Stack")
extern class Stack<T> {
	var Count(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<T>):Void {})
	function new(capacity:Int):Void;
	function Clear():Void;
	function Contains(item:T):Bool;
	function CopyTo(array:cs.NativeArray<T>, arrayIndex:Int):Void;
	function GetEnumerator():cs.system.collections.generic.Stack_Enumerator<T>;
	function Peek():T;
	function Pop():T;
	function Push(item:T):Void;
	function ToArray():cs.NativeArray<T>;
	function TrimExcess():Void;
	function TryPeek(result:cs.Ref<T>):Bool;
	function TryPop(result:cs.Ref<T>):Bool;
}
