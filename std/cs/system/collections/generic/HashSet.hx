package cs.system.collections.generic;

@:native("System.Collections.Generic.HashSet")
extern class HashSet<T> {
	var Comparer(default, never):cs.system.collections.generic.IEqualityComparer<T>;
	var Count(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<T>):Void {})
	@:overload(function(comparer:cs.system.collections.generic.IEqualityComparer<T>):Void {})
	@:overload(function(capacity:Int):Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<T>, comparer:cs.system.collections.generic.IEqualityComparer<T>):Void {})
	function new(capacity:Int, comparer:cs.system.collections.generic.IEqualityComparer<T>):Void;
	static function CreateSetComparer<T>():cs.system.collections.generic.IEqualityComparer<cs.system.collections.generic.HashSet<T>>;
	function Add(item:T):Bool;
	function Clear():Void;
	function Contains(item:T):Bool;
	@:overload(function(array:cs.NativeArray<T>):Void {})
	@:overload(function(array:cs.NativeArray<T>, arrayIndex:Int):Void {})
	function CopyTo(array:cs.NativeArray<T>, arrayIndex:Int, count:Int):Void;
	function EnsureCapacity(capacity:Int):Int;
	function ExceptWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function GetEnumerator():cs.system.collections.generic.HashSet_Enumerator<T>;
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	function IntersectWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function IsProperSubsetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsProperSupersetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsSubsetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsSupersetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function OnDeserialization(sender:Dynamic):Void;
	function Overlaps(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function Remove(item:T):Bool;
	function RemoveWhere(match:cs.system.Predicate<T>):Int;
	function SetEquals(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function SymmetricExceptWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function TrimExcess():Void;
	function TryGetValue(equalValue:T, actualValue:cs.Ref<T>):Bool;
	function UnionWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
}
