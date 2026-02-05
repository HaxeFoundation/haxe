package cs.system.linq;

@:native("System.Linq.IOrderedEnumerable")
extern interface IOrderedEnumerable<TElement> extends cs.system.collections.generic.IEnumerable<TElement> extends cs.system.collections.IEnumerable {
	function CreateOrderedEnumerable<TKey>(keySelector:cs.system.Func_2<TElement, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>, descending:Bool):cs.system.linq.IOrderedEnumerable<TElement>;
}
