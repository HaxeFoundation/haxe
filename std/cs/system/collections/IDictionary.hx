package cs.system.collections;

@:native("System.Collections.IDictionary")
extern interface IDictionary {
	function GetEnumerator():IDictionaryEnumerator;
}
