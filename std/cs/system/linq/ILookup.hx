package cs.system.linq;

@:native("System.Linq.ILookup")
extern interface ILookup<TKey, TElement> extends cs.system.collections.generic.IEnumerable<cs.system.linq.IGrouping<TKey, TElement>> extends cs.system.collections.IEnumerable {
	var Count(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:TKey):cs.system.collections.generic.IEnumerable<TElement>;
	function Contains(key:TKey):Bool;
}
