package cs.system;

/** The exception that is thrown when an invalid Uniform Resource Identifier (URI) is detected. */
@:native("System.UriFormatException")
extern class UriFormatException extends cs.system.FormatException {
	@:overload(function():Void {})
	@:overload(function(textString:String):Void {})
	function new(textString:String, e:cs.system.Exception):Void;
}
