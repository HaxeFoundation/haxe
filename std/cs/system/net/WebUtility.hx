package cs.system.net;

/** Provides methods for encoding and decoding URLs when processing Web requests. */
@:native("System.Net.WebUtility")
extern class WebUtility {
	@:overload(function(value:String):String {})
	/**
	 * Converts a string that has been HTML-encoded for HTTP transmission into a
	 * decoded string.
	 * @param value The string to decode.
	 * @return A decoded string.
	 */
	static function HtmlDecode(value:String, output:cs.system.io.TextWriter):Void;
	@:overload(function(value:String):String {})
	/**
	 * Converts a string to an HTML-encoded string.
	 * @param value The string to encode.
	 * @return An encoded string.
	 */
	static function HtmlEncode(value:String, output:cs.system.io.TextWriter):Void;
	/**
	 * Converts a string that has been encoded for transmission in a URL into a decoded
	 * string.
	 * @param encodedValue A URL-encoded string to decode.
	 * @return A decoded string.
	 */
	static function UrlDecode(encodedValue:String):String;
	/**
	 * Converts an encoded byte array that has been encoded for transmission in a URL
	 * into a decoded byte array.
	 * @param encodedValue A URL-encoded  array to decode.
	 * @param offset The offset, in bytes, from the start of the  array to decode.
	 * @param count The count, in bytes, to decode from the  array.
	 * @return A decoded  array.
	 */
	static function UrlDecodeToBytes(encodedValue:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):cs.NativeArray<cs.UInt8>;
	/**
	 * Converts a text string into a URL-encoded string.
	 * @param value The text to URL-encode.
	 * @return A URL-encoded string.
	 */
	static function UrlEncode(value:String):String;
	/**
	 * Converts a byte array into a URL-encoded byte array.
	 * @param value The  array to URL-encode.
	 * @param offset The offset, in bytes, from the start of the  array to encode.
	 * @param count The count, in bytes, to encode from the  array.
	 * @return An encoded  array.
	 */
	static function UrlEncodeToBytes(value:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):cs.NativeArray<cs.UInt8>;
}
