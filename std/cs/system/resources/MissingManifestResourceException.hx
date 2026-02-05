package cs.system.resources;

/** The exception that is thrown if the main assembly does not contain the resources for the neutral culture, and an appropriate satellite assembly is missing. */
@:native("System.Resources.MissingManifestResourceException")
extern class MissingManifestResourceException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
