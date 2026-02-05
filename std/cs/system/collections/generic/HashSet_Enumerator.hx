package cs.system.collections.generic;

@:native("System.Collections.Generic.HashSet`1.Enumerator")
extern class HashSet_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):T;
	function Dispose():Void;
	function MoveNext():Bool;
}
