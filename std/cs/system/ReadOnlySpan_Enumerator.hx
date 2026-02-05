package cs.system;

@:native("System.ReadOnlySpan`1.Enumerator")
extern class ReadOnlySpan_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):T;
	function MoveNext():Bool;
}
