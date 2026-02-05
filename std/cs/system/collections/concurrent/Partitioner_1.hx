package cs.system.collections.concurrent;

/** Provides common partitioning strategies for arrays, lists, and enumerables. */
@:native("System.Collections.Concurrent.Partitioner`1")
extern class Partitioner_1<TSource> {
	var SupportsDynamicPartitions(default, never):Bool;
	function GetDynamicPartitions():cs.system.collections.generic.IEnumerable<TSource>;
	function GetPartitions(partitionCount:Int):cs.system.collections.generic.IList<cs.system.collections.generic.IEnumerator<TSource>>;
}
