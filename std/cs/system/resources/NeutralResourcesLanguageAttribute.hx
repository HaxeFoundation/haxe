package cs.system.resources;

/** Informs the resource manager of an app's default culture. This class cannot be inherited. */
@:native("System.Resources.NeutralResourcesLanguageAttribute")
extern class NeutralResourcesLanguageAttribute extends cs.system.Attribute {
	/**
	 * Gets the culture name.
	 * @return The name of the default culture for the main assembly.
	 */
	var CultureName(default, never):String;
	/**
	 * Gets the location for the  class to use to retrieve neutral resources by using
	 * the resource fallback process.
	 * @return One of the enumeration values that indicates the location (main assembly
	 * or satellite) from which to retrieve neutral resources.
	 */
	var Location(default, never):cs.system.resources.UltimateResourceFallbackLocation;
	@:overload(function(cultureName:String):Void {})
	function new(cultureName:String, location:cs.system.resources.UltimateResourceFallbackLocation):Void;
}
