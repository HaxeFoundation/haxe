package cs.system;

/** Provides static methods for creating tuple objects. */
@:native("System.Tuple`1")
extern class Tuple_1<T1> {
	var Item1(default, never):T1;
	function new(item1:T1):Void;
	function Equals(obj:Dynamic):Bool;
	function GetHashCode():Int;
	function ToString():String;
}
