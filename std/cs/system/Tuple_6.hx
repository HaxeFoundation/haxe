package cs.system;

/** Provides static methods for creating tuple objects. */
@:native("System.Tuple`6")
extern class Tuple_6<T1, T2, T3, T4, T5, T6> {
	var Item1(default, never):T1;
	var Item2(default, never):T2;
	var Item3(default, never):T3;
	var Item4(default, never):T4;
	var Item5(default, never):T5;
	var Item6(default, never):T6;
	function new(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5, item6:T6):Void;
	function Equals(obj:Dynamic):Bool;
	function GetHashCode():Int;
	function ToString():String;
}
