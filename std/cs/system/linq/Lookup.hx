package cs.system.linq;

@:native("System.Linq.Lookup")
extern class Lookup<TKey, TElement> {
	var Count(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:TKey):cs.system.collections.generic.IEnumerable<TElement>;
	function ApplyResultSelector<TResult>(resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TElement>, TResult>):cs.system.collections.generic.IEnumerable<TResult>;
	function Contains(key:TKey):Bool;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<cs.system.linq.IGrouping<TKey, TElement>>;
}
