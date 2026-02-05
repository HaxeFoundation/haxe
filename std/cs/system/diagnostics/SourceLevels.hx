package cs.system.diagnostics;

/** Specifies the levels of trace messages filtered by the source switch and event type filter. */
@:native("System.Diagnostics.SourceLevels")
extern enum abstract SourceLevels(Int) {
	var ActivityTracing = 65280;
	var All = -1;
	var Critical = 1;
	var Error = 3;
	var Information = 15;
	var Off = 0;
	var Verbose = 31;
	var Warning = 7;
	@:op(A | B) static function or(lhs:SourceLevels, rhs:SourceLevels):SourceLevels;
	@:op(A & B) static function and(lhs:SourceLevels, rhs:SourceLevels):SourceLevels;
	@:op(A ^ B) static function xor(lhs:SourceLevels, rhs:SourceLevels):SourceLevels;
	@:op(~A) static function complement(value:SourceLevels):SourceLevels;
}
