package cs.system.diagnostics.tracing;

/** Specifies the user-defined tag that is placed on fields of user-defined types that are passed as  payloads through the . */
@:native("System.Diagnostics.Tracing.EventFieldTags")
extern enum abstract EventFieldTags(Int) {
	var None = 0;
	@:op(A | B) static function or(lhs:EventFieldTags, rhs:EventFieldTags):EventFieldTags;
	@:op(A & B) static function and(lhs:EventFieldTags, rhs:EventFieldTags):EventFieldTags;
	@:op(A ^ B) static function xor(lhs:EventFieldTags, rhs:EventFieldTags):EventFieldTags;
	@:op(~A) static function complement(value:EventFieldTags):EventFieldTags;
}
