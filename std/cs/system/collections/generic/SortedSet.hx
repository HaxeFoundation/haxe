package cs.system.collections.generic;

@:native("System.Collections.Generic.SortedSet")
extern class SortedSet<T> {
	var Comparer(default, never):cs.system.collections.generic.IComparer<T>;
	var Count(default, never):Int;
	var Max(default, never):T;
	var Min(default, never):T;
	@:overload(function():Void {})
	@:overload(function(comparer:cs.system.collections.generic.IComparer<T>):Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<T>):Void {})
	function new(collection:cs.system.collections.generic.IEnumerable<T>, comparer:cs.system.collections.generic.IComparer<T>):Void;
	@:overload(function<T>():cs.system.collections.generic.IEqualityComparer<cs.system.collections.generic.SortedSet<T>> {})
	static function CreateSetComparer<T>(memberEqualityComparer:cs.system.collections.generic.IEqualityComparer<T>):cs.system.collections.generic.IEqualityComparer<cs.system.collections.generic.SortedSet<T>>;
	function Add(item:T):Bool;
	function Clear():Void;
	function Contains(item:T):Bool;
	@:overload(function(array:cs.NativeArray<T>):Void {})
	@:overload(function(array:cs.NativeArray<T>, index:Int):Void {})
	function CopyTo(array:cs.NativeArray<T>, index:Int, count:Int):Void;
	function ExceptWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function GetEnumerator():cs.system.collections.generic.SortedSet_Enumerator<T>;
	function GetViewBetween(lowerValue:T, upperValue:T):cs.system.collections.generic.SortedSet<T>;
	function IntersectWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function IsProperSubsetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsProperSupersetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsSubsetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsSupersetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function Overlaps(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function Remove(item:T):Bool;
	function RemoveWhere(match:cs.system.Predicate<T>):Int;
	function Reverse():cs.system.collections.generic.IEnumerable<T>;
	function SetEquals(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function SymmetricExceptWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function TryGetValue(equalValue:T, actualValue:cs.Ref<T>):Bool;
	function UnionWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
}
