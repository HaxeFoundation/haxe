package cs.system.collections.concurrent;

/** Provides common partitioning strategies for arrays, lists, and enumerables. */
@:native("System.Collections.Concurrent.Partitioner")
extern class Partitioner {
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.concurrent.OrderablePartitioner<TSource> {})
	@:overload(function(fromInclusive:Int, toExclusive:Int):cs.system.collections.concurrent.OrderablePartitioner<cs.system.Tuple_2<Int, Int>> {})
	@:overload(function(fromInclusive:haxe.Int64, toExclusive:haxe.Int64):cs.system.collections.concurrent.OrderablePartitioner<cs.system.Tuple_2<haxe.Int64, haxe.Int64>> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, partitionerOptions:cs.system.collections.concurrent.EnumerablePartitionerOptions):cs.system.collections.concurrent.OrderablePartitioner<TSource> {})
	@:overload(function<TSource>(list:cs.system.collections.generic.IList<TSource>, loadBalance:Bool):cs.system.collections.concurrent.OrderablePartitioner<TSource> {})
	@:overload(function<TSource>(array:cs.NativeArray<TSource>, loadBalance:Bool):cs.system.collections.concurrent.OrderablePartitioner<TSource> {})
	@:overload(function(fromInclusive:Int, toExclusive:Int, rangeSize:Int):cs.system.collections.concurrent.OrderablePartitioner<cs.system.Tuple_2<Int, Int>> {})
	/**
	 * Creates a partitioner that chunks the user-specified range.
	 * @param fromInclusive The lower, inclusive bound of the range.
	 * @param toExclusive The upper, exclusive bound of the range.
	 * @return A partitioner.
	 */
	static function Create(fromInclusive:haxe.Int64, toExclusive:haxe.Int64, rangeSize:haxe.Int64):cs.system.collections.concurrent.OrderablePartitioner<cs.system.Tuple_2<haxe.Int64, haxe.Int64>>;
}
