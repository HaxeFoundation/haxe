package cs.system.net.http.headers;

/** Represents a media type with an additional quality factor used in a Content-Type header. */
@:native("System.Net.Http.Headers.MediaTypeWithQualityHeaderValue")
extern class MediaTypeWithQualityHeaderValue extends cs.system.net.http.headers.MediaTypeHeaderValue {
	/**
	 * Gets or sets the quality value for the .
	 * @return The quality value for the  object.
	 */
	var Quality(default, default):Null<Float>;
	@:overload(function(mediaType:String):Void {})
	function new(mediaType:String, quality:Float):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents media type with quality header value
	 * information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.MediaTypeWithQualityHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.MediaTypeWithQualityHeaderValue>):Bool;
}
