package cs.system;

/** Provides static methods for creating tuple objects. */
@:native("System.Tuple`3")
extern class Tuple_3<T1, T2, T3> {
	var Item1(default, never):T1;
	var Item2(default, never):T2;
	var Item3(default, never):T3;
	function new(item1:T1, item2:T2, item3:T3):Void;
	function Equals(obj:Dynamic):Bool;
	function GetHashCode():Int;
	function ToString():String;
}
