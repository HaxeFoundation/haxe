package cs.system.diagnostics;

/** Specifies trace data options to be written to the trace output. */
@:native("System.Diagnostics.TraceOptions")
extern enum abstract TraceOptions(Int) {
	var Callstack = 32;
	var DateTime = 2;
	var LogicalOperationStack = 1;
	var None = 0;
	var ProcessId = 8;
	var ThreadId = 16;
	var Timestamp = 4;
	@:op(A | B) static function or(lhs:TraceOptions, rhs:TraceOptions):TraceOptions;
	@:op(A & B) static function and(lhs:TraceOptions, rhs:TraceOptions):TraceOptions;
	@:op(A ^ B) static function xor(lhs:TraceOptions, rhs:TraceOptions):TraceOptions;
	@:op(~A) static function complement(value:TraceOptions):TraceOptions;
}
