package cs.system.linq;

@:native("System.Linq.OrderedParallelQuery")
extern class OrderedParallelQuery<TSource> extends cs.system.linq.ParallelQuery_1<T0> {
	function GetEnumerator():cs.system.collections.generic.IEnumerator<TSource>;
}
