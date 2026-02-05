package cs.system;

/** Provides static methods for creating tuple objects. */
@:native("System.Tuple`2")
extern class Tuple_2<T1, T2> {
	var Item1(default, never):T1;
	var Item2(default, never):T2;
	function new(item1:T1, item2:T2):Void;
	function Equals(obj:Dynamic):Bool;
	function GetHashCode():Int;
	function ToString():String;
}
