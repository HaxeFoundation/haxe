package cs.system.collections.concurrent;

/** Specifies options to control the buffering behavior of a partitioner */
@:native("System.Collections.Concurrent.EnumerablePartitionerOptions")
extern enum abstract EnumerablePartitionerOptions(Int) {
	var NoBuffering = 1;
	var None = 0;
	@:op(A | B) static function or(lhs:EnumerablePartitionerOptions, rhs:EnumerablePartitionerOptions):EnumerablePartitionerOptions;
	@:op(A & B) static function and(lhs:EnumerablePartitionerOptions, rhs:EnumerablePartitionerOptions):EnumerablePartitionerOptions;
	@:op(A ^ B) static function xor(lhs:EnumerablePartitionerOptions, rhs:EnumerablePartitionerOptions):EnumerablePartitionerOptions;
	@:op(~A) static function complement(value:EnumerablePartitionerOptions):EnumerablePartitionerOptions;
}
