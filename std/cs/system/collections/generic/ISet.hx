package cs.system.collections.generic;

@:native("System.Collections.Generic.ISet")
extern interface ISet<T> extends cs.system.collections.generic.ICollection<T> extends cs.system.collections.generic.IEnumerable<T> extends cs.system.collections.IEnumerable {
	function Add(item:T):Bool;
	function ExceptWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function IntersectWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function IsProperSubsetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsProperSupersetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsSubsetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function IsSupersetOf(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function Overlaps(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function SetEquals(other:cs.system.collections.generic.IEnumerable<T>):Bool;
	function SymmetricExceptWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
	function UnionWith(other:cs.system.collections.generic.IEnumerable<T>):Void;
}
