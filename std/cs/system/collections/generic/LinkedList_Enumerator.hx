package cs.system.collections.generic;

@:native("System.Collections.Generic.LinkedList`1.Enumerator")
extern class LinkedList_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):T;
	function Dispose():Void;
	function MoveNext():Bool;
}
