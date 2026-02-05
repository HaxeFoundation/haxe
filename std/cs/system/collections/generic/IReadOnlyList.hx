package cs.system.collections.generic;

@:native("System.Collections.Generic.IReadOnlyList")
extern interface IReadOnlyList<T> extends cs.system.collections.generic.IEnumerable<T> extends cs.system.collections.IEnumerable extends cs.system.collections.generic.IReadOnlyCollection<T> {
	@:native("get_Item")
	function get_Item(index0:Int):T;
}
