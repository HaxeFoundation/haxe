package cs.system.diagnostics.tracing;

/** Specifies how the ETW manifest for the event source is generated. */
@:native("System.Diagnostics.Tracing.EventManifestOptions")
extern enum abstract EventManifestOptions(Int) {
	var AllCultures = 2;
	var AllowEventSourceOverride = 8;
	var None = 0;
	var OnlyIfNeededForRegistration = 4;
	var Strict = 1;
	@:op(A | B) static function or(lhs:EventManifestOptions, rhs:EventManifestOptions):EventManifestOptions;
	@:op(A & B) static function and(lhs:EventManifestOptions, rhs:EventManifestOptions):EventManifestOptions;
	@:op(A ^ B) static function xor(lhs:EventManifestOptions, rhs:EventManifestOptions):EventManifestOptions;
	@:op(~A) static function complement(value:EventManifestOptions):EventManifestOptions;
}
