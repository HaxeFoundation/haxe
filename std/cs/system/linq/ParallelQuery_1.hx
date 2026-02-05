package cs.system.linq;

/** Represents a parallel sequence. */
@:native("System.Linq.ParallelQuery`1")
extern class ParallelQuery_1<TSource> extends cs.system.linq.ParallelQuery {
	function GetEnumerator():cs.system.collections.generic.IEnumerator<TSource>;
}
