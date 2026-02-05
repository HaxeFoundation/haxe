package cs.system.diagnostics.tracing;

/** Specifies the tracking of activity start and stop events. You should only use the lower 24 bits. For more information, see  and . */
@:native("System.Diagnostics.Tracing.EventTags")
extern enum abstract EventTags(Int) {
	var None = 0;
	@:op(A | B) static function or(lhs:EventTags, rhs:EventTags):EventTags;
	@:op(A & B) static function and(lhs:EventTags, rhs:EventTags):EventTags;
	@:op(A ^ B) static function xor(lhs:EventTags, rhs:EventTags):EventTags;
	@:op(~A) static function complement(value:EventTags):EventTags;
}
