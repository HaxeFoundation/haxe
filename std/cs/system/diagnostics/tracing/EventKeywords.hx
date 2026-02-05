package cs.system.diagnostics.tracing;

/** Defines the standard keywords that apply to events. */
@:native("System.Diagnostics.Tracing.EventKeywords")
extern enum abstract EventKeywords(Int) {
	var All = -1;
	var AuditFailure = 0;
	var AuditSuccess = 0;
	var CorrelationHint = 0;
	var EventLogClassic = 0;
	var MicrosoftTelemetry = 0;
	var None = 0;
	var Sqm = 0;
	var WdiContext = 0;
	var WdiDiagnostic = 0;
	@:op(A | B) static function or(lhs:EventKeywords, rhs:EventKeywords):EventKeywords;
	@:op(A & B) static function and(lhs:EventKeywords, rhs:EventKeywords):EventKeywords;
	@:op(A ^ B) static function xor(lhs:EventKeywords, rhs:EventKeywords):EventKeywords;
	@:op(~A) static function complement(value:EventKeywords):EventKeywords;
}
