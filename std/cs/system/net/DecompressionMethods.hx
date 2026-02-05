package cs.system.net;

/** Represents the file compression and decompression encoding format to be used to compress the data received in response to an . */
@:native("System.Net.DecompressionMethods")
extern enum abstract DecompressionMethods(Int) {
	var Deflate = 2;
	var GZip = 1;
	var None = 0;
	@:op(A | B) static function or(lhs:DecompressionMethods, rhs:DecompressionMethods):DecompressionMethods;
	@:op(A & B) static function and(lhs:DecompressionMethods, rhs:DecompressionMethods):DecompressionMethods;
	@:op(A ^ B) static function xor(lhs:DecompressionMethods, rhs:DecompressionMethods):DecompressionMethods;
	@:op(~A) static function complement(value:DecompressionMethods):DecompressionMethods;
}
