package cs.system;

@:native("System.ArraySegment`1.Enumerator")
extern class ArraySegment_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):T;
	function Dispose():Void;
	function MoveNext():Bool;
}
