package cs.system.text;

/** The exception that is thrown when a decoder fallback operation fails. This class cannot be inherited. */
@:native("System.Text.DecoderFallbackException")
extern class DecoderFallbackException extends cs.system.ArgumentException {
	/**
	 * Gets the input byte sequence that caused the exception.
	 * @return The input byte array that cannot be decoded.
	 */
	var BytesUnknown(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the index position in the input byte sequence of the byte that caused the
	 * exception.
	 * @return The index position in the input byte array of the byte that cannot be
	 * decoded. The index position is zero-based.
	 */
	var Index(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, bytesUnknown:cs.NativeArray<cs.UInt8>, index:Int):Void;
}
