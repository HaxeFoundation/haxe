package cs.system.collections.generic;

@:native("System.Collections.Generic.LinkedList")
extern class LinkedList<T> {
	var Count(default, never):Int;
	var First(default, never):cs.system.collections.generic.LinkedListNode<T>;
	var Last(default, never):cs.system.collections.generic.LinkedListNode<T>;
	@:overload(function():Void {})
	function new(collection:cs.system.collections.generic.IEnumerable<T>):Void;
	@:overload(function(node:cs.system.collections.generic.LinkedListNode<T>, newNode:cs.system.collections.generic.LinkedListNode<T>):Void {})
	function AddAfter(node:cs.system.collections.generic.LinkedListNode<T>, value:T):cs.system.collections.generic.LinkedListNode<T>;
	@:overload(function(node:cs.system.collections.generic.LinkedListNode<T>, newNode:cs.system.collections.generic.LinkedListNode<T>):Void {})
	function AddBefore(node:cs.system.collections.generic.LinkedListNode<T>, value:T):cs.system.collections.generic.LinkedListNode<T>;
	@:overload(function(node:cs.system.collections.generic.LinkedListNode<T>):Void {})
	function AddFirst(value:T):cs.system.collections.generic.LinkedListNode<T>;
	@:overload(function(node:cs.system.collections.generic.LinkedListNode<T>):Void {})
	function AddLast(value:T):cs.system.collections.generic.LinkedListNode<T>;
	function Clear():Void;
	function Contains(value:T):Bool;
	function CopyTo(array:cs.NativeArray<T>, index:Int):Void;
	function Find(value:T):cs.system.collections.generic.LinkedListNode<T>;
	function FindLast(value:T):cs.system.collections.generic.LinkedListNode<T>;
	function GetEnumerator():cs.system.collections.generic.LinkedList_Enumerator<T>;
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	function OnDeserialization(sender:Dynamic):Void;
	@:overload(function(node:cs.system.collections.generic.LinkedListNode<T>):Void {})
	function Remove(value:T):Bool;
	function RemoveFirst():Void;
	function RemoveLast():Void;
}
