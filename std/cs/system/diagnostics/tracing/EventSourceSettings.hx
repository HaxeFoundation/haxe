package cs.system.diagnostics.tracing;

/** Specifies configuration options for an event source. */
@:native("System.Diagnostics.Tracing.EventSourceSettings")
extern enum abstract EventSourceSettings(Int) {
	var Default = 0;
	var EtwManifestEventFormat = 4;
	var EtwSelfDescribingEventFormat = 8;
	var ThrowOnEventWriteErrors = 1;
	@:op(A | B) static function or(lhs:EventSourceSettings, rhs:EventSourceSettings):EventSourceSettings;
	@:op(A & B) static function and(lhs:EventSourceSettings, rhs:EventSourceSettings):EventSourceSettings;
	@:op(A ^ B) static function xor(lhs:EventSourceSettings, rhs:EventSourceSettings):EventSourceSettings;
	@:op(~A) static function complement(value:EventSourceSettings):EventSourceSettings;
}
