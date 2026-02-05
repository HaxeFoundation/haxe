package cs.system;

@:native("System.Span`1.Enumerator")
extern class Span_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):T;
	function MoveNext():Bool;
}
