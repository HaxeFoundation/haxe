package cs.system.collections.generic;

@:native("System.Collections.Generic.EqualityComparer")
extern class EqualityComparer<T> {
	static var Default(default, never):cs.system.collections.generic.EqualityComparer<Dynamic>;
	function Equals(x:T, y:T):Bool;
	function GetHashCode(obj:T):Int;
}
