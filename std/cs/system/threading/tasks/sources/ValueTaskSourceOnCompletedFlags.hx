package cs.system.threading.tasks.sources;

/** Provides flags passed from  and  to the  method to control the behavior of a continuation. */
@:native("System.Threading.Tasks.Sources.ValueTaskSourceOnCompletedFlags")
extern enum abstract ValueTaskSourceOnCompletedFlags(Int) {
	var FlowExecutionContext = 2;
	var None = 0;
	var UseSchedulingContext = 1;
	@:op(A | B) static function or(lhs:ValueTaskSourceOnCompletedFlags, rhs:ValueTaskSourceOnCompletedFlags):ValueTaskSourceOnCompletedFlags;
	@:op(A & B) static function and(lhs:ValueTaskSourceOnCompletedFlags, rhs:ValueTaskSourceOnCompletedFlags):ValueTaskSourceOnCompletedFlags;
	@:op(A ^ B) static function xor(lhs:ValueTaskSourceOnCompletedFlags, rhs:ValueTaskSourceOnCompletedFlags):ValueTaskSourceOnCompletedFlags;
	@:op(~A) static function complement(value:ValueTaskSourceOnCompletedFlags):ValueTaskSourceOnCompletedFlags;
}
