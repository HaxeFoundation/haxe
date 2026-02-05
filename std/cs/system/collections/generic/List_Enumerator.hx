package cs.system.collections.generic;

@:native("System.Collections.Generic.List`1.Enumerator")
extern class List_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):T;
	function Dispose():Void;
	function MoveNext():Bool;
}
