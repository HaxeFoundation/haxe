package cs.system.collections.generic;

@:native("System.Collections.Generic.IEqualityComparer")
extern interface IEqualityComparer<T> {
	function Equals(x:T, y:T):Bool;
	function GetHashCode(obj:T):Int;
}
