package cs.system.resources;

/** The exception that is thrown when the satellite assembly for the resources of the default culture is missing. */
@:native("System.Resources.MissingSatelliteAssemblyException")
extern class MissingSatelliteAssemblyException extends cs.system.SystemException {
	/**
	 * Gets the name of the default culture.
	 * @return The name of the default culture.
	 */
	var CultureName(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(message:String, cultureName:String):Void;
}
