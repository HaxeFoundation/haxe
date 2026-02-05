package cs.system.diagnostics.tracing;

/** Specifies the tracking of activity start and stop events. */
@:native("System.Diagnostics.Tracing.EventActivityOptions")
extern enum abstract EventActivityOptions(Int) {
	var Detachable = 8;
	var Disable = 2;
	var None = 0;
	var Recursive = 4;
	@:op(A | B) static function or(lhs:EventActivityOptions, rhs:EventActivityOptions):EventActivityOptions;
	@:op(A & B) static function and(lhs:EventActivityOptions, rhs:EventActivityOptions):EventActivityOptions;
	@:op(A ^ B) static function xor(lhs:EventActivityOptions, rhs:EventActivityOptions):EventActivityOptions;
	@:op(~A) static function complement(value:EventActivityOptions):EventActivityOptions;
}
