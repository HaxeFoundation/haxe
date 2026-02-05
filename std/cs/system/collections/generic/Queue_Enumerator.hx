package cs.system.collections.generic;

@:native("System.Collections.Generic.Queue`1.Enumerator")
extern class Queue_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):T;
	function Dispose():Void;
	function MoveNext():Bool;
}
