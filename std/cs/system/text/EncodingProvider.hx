package cs.system.text;

/** Provides the base class for an encoding provider, which supplies encodings that are unavailable on a particular platform. */
@:native("System.Text.EncodingProvider")
extern class EncodingProvider {
	function new():Void;
	@:overload(function(codepage:Int):cs.system.text.Encoding {})
	@:overload(function(name:String):cs.system.text.Encoding {})
	@:overload(function(codepage:Int, encoderFallback:cs.system.text.EncoderFallback, decoderFallback:cs.system.text.DecoderFallback):cs.system.text.Encoding {})
	/**
	 * Returns the encoding associated with the specified code page identifier.
	 * @param codepage The code page identifier of the requested encoding.
	 * @return The encoding that is associated with the specified code page, or  if
	 * this  cannot return a valid encoding that corresponds to .
	 */
	function GetEncoding(name:String, encoderFallback:cs.system.text.EncoderFallback, decoderFallback:cs.system.text.DecoderFallback):cs.system.text.Encoding;
}
