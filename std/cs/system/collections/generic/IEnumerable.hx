package cs.system.collections.generic;

@:native("System.Collections.Generic.IEnumerable")
extern interface IEnumerable<T> extends cs.system.collections.IEnumerable {
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
}
