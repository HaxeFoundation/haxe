package cs.system.web;

/** Provides methods for encoding and decoding URLs when processing Web requests. This class cannot be inherited. */
@:native("System.Web.HttpUtility")
extern class HttpUtility {
	function new():Void;
	@:overload(function(s:String):String {})
	/**
	 * Minimally converts a string to an HTML-encoded string.
	 * @param s The string to encode.
	 * @return An encoded string.
	 */
	static function HtmlAttributeEncode(s:String, output:cs.system.io.TextWriter):Void;
	@:overload(function(s:String):String {})
	/**
	 * Converts a string that has been HTML-encoded for HTTP transmission into a
	 * decoded string.
	 * @param s The string to decode.
	 * @return A decoded string.
	 */
	static function HtmlDecode(s:String, output:cs.system.io.TextWriter):Void;
	@:overload(function(value:Dynamic):String {})
	@:overload(function(s:String):String {})
	/**
	 * Converts an object's string representation into an HTML-encoded string, and
	 * returns the encoded string.
	 * @param value An object.
	 * @return An encoded string.
	 */
	static function HtmlEncode(s:String, output:cs.system.io.TextWriter):Void;
	@:overload(function(value:String):String {})
	/**
	 * Encodes a string.
	 * @param value A string to encode.
	 * @return An encoded string.
	 */
	static function JavaScriptStringEncode(value:String, addDoubleQuotes:Bool):String;
	@:overload(function(query:String):cs.system.collections.specialized.NameValueCollection {})
	/**
	 * Parses a query string into a  using  encoding.
	 * @param query The query string to parse.
	 * @return A  of query parameters and values.
	 */
	static function ParseQueryString(query:String, encoding:cs.system.text.Encoding):cs.system.collections.specialized.NameValueCollection;
	@:overload(function(str:String):String {})
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>, e:cs.system.text.Encoding):String {})
	@:overload(function(str:String, e:cs.system.text.Encoding):String {})
	/**
	 * Converts a URL-encoded byte array into a decoded string using the specified
	 * encoding object, starting at the specified position in the array, and continuing
	 * for the specified number of bytes.
	 * @param bytes The array of bytes to decode.
	 * @param offset The position in the byte to begin decoding.
	 * @param count The number of bytes to decode.
	 * @param e The  object that specifies the decoding scheme.
	 * @return A decoded string.
	 */
	static function UrlDecode(bytes:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, e:cs.system.text.Encoding):String;
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	@:overload(function(str:String):cs.NativeArray<cs.UInt8> {})
	@:overload(function(str:String, e:cs.system.text.Encoding):cs.NativeArray<cs.UInt8> {})
	/**
	 * Converts a URL-encoded array of bytes into a decoded array of bytes.
	 * @param bytes The array of bytes to decode.
	 * @return A decoded array of bytes.
	 */
	static function UrlDecodeToBytes(bytes:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):cs.NativeArray<cs.UInt8>;
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>):String {})
	@:overload(function(str:String):String {})
	@:overload(function(str:String, e:cs.system.text.Encoding):String {})
	/**
	 * Converts a byte array into an encoded URL string.
	 * @param bytes The array of bytes to encode.
	 * @return An encoded string.
	 */
	static function UrlEncode(bytes:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):String;
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	@:overload(function(str:String):cs.NativeArray<cs.UInt8> {})
	@:overload(function(str:String, e:cs.system.text.Encoding):cs.NativeArray<cs.UInt8> {})
	/**
	 * Converts an array of bytes into a URL-encoded array of bytes.
	 * @param bytes The array of bytes to encode.
	 * @return An encoded array of bytes.
	 */
	static function UrlEncodeToBytes(bytes:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):cs.NativeArray<cs.UInt8>;
	/**
	 * Converts a string into a Unicode string.
	 * @param str The string to convert.
	 * @return A Unicode string in % notation.
	 */
	static function UrlEncodeUnicode(str:String):String;
	/**
	 * Converts a Unicode string into an array of bytes.
	 * @param str The string to convert.
	 * @return A byte array.
	 */
	static function UrlEncodeUnicodeToBytes(str:String):cs.NativeArray<cs.UInt8>;
	/**
	 * Do not use; intended only for browser compatibility. Use .
	 * @param str The text to encode.
	 * @return The encoded text.
	 */
	static function UrlPathEncode(str:String):String;
}
