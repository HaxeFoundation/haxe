package cs.system.net.http.headers;

/** Represents an Accept-Encoding header value.with optional quality factor. */
@:native("System.Net.Http.Headers.TransferCodingWithQualityHeaderValue")
extern class TransferCodingWithQualityHeaderValue extends cs.system.net.http.headers.TransferCodingHeaderValue {
	/**
	 * Gets the quality factor from the .
	 * @return The quality factor from the .
	 */
	var Quality(default, default):Null<Float>;
	@:overload(function(value:String):Void {})
	function new(value:String, quality:Float):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents transfer-coding value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.TransferCodingWithQualityHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.TransferCodingWithQualityHeaderValue>):Bool;
}
