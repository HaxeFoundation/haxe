package cs.system.collections;

@:native("System.Collections.IDictionaryEnumerator")
extern interface IDictionaryEnumerator {
	function MoveNext():Bool;
	var Key(default, never):Dynamic;
	var Value(default, never):Dynamic;
}
