package cs.system.collections.concurrent;

@:native("System.Collections.Concurrent.OrderablePartitioner")
extern class OrderablePartitioner<TSource> extends cs.system.collections.concurrent.Partitioner_1<T0> {
	var KeysNormalized(default, never):Bool;
	var KeysOrderedAcrossPartitions(default, never):Bool;
	var KeysOrderedInEachPartition(default, never):Bool;
	function GetDynamicPartitions():cs.system.collections.generic.IEnumerable<TSource>;
	function GetOrderableDynamicPartitions():cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<haxe.Int64, TSource>>;
	function GetOrderablePartitions(partitionCount:Int):cs.system.collections.generic.IList<cs.system.collections.generic.IEnumerator<cs.system.collections.generic.KeyValuePair_2<haxe.Int64, TSource>>>;
	function GetPartitions(partitionCount:Int):cs.system.collections.generic.IList<cs.system.collections.generic.IEnumerator<TSource>>;
}
