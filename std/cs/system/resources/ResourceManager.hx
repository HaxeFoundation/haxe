package cs.system.resources;

/** Represents a resource manager that provides convenient access to culture-specific resources at run time. */
@:native("System.Resources.ResourceManager")
extern class ResourceManager {
	/** Specifies the version of resource file headers that the current implementation of  can interpret and produce. */
	static var HeaderVersionNumber(default, never):Int;
	/** Holds the number used to identify resource files. */
	static var MagicNumber(default, never):Int;
	/**
	 * Gets the root name of the resource files that the  searches for resources.
	 * @return The root name of the resource files that the  searches for resources.
	 */
	var BaseName(default, never):String;
	/**
	 * Gets or sets the location from which to retrieve default fallback resources.
	 * @return One of the enumeration values that specifies where the resource manager
	 * can look for fallback resources.
	 */
	var FallbackLocation(default, default):cs.system.resources.UltimateResourceFallbackLocation;
	/**
	 * Gets or sets a value that indicates whether the resource manager allows
	 * case-insensitive resource lookups in the  and  methods.
	 * @return to ignore case during resource lookup; otherwise, .
	 */
	var IgnoreCase(default, default):Bool;
	/**
	 * Gets the type of the resource set object that the resource manager uses to
	 * construct a  object.
	 * @return The type of the resource set object that the resource manager uses to
	 * construct a  object.
	 */
	var ResourceSetType(default, never):cs.system.Type;
	@:overload(function(resourceSource:cs.system.Type):Void {})
	@:overload(function(baseName:String, assembly:cs.system.reflection.Assembly):Void {})
	function new(baseName:String, assembly:cs.system.reflection.Assembly, usingResourceSet:cs.system.Type):Void;
	/**
	 * Returns a  object that searches a specific directory instead of an assembly
	 * manifest for resources.
	 * @param baseName The root name of the resources. For example, the root name for
	 * the resource file named "MyResource.en-US.resources" is "MyResource".
	 * @param resourceDir The name of the directory to search for the resources.  can
	 * be an absolute path or a relative path from the application directory.
	 * @param usingResourceSet The type of the custom  to use. If , the default runtime
	 * object is used.
	 * @return A new instance of a resource manager that searches the specified
	 * directory instead of an assembly manifest for resources.
	 */
	static function CreateFileBasedResourceManager(baseName:String, resourceDir:String, usingResourceSet:cs.system.Type):cs.system.resources.ResourceManager;
	@:overload(function(name:String):Dynamic {})
	/**
	 * Returns the value of the specified non-string resource.
	 * @param name The name of the resource to get.
	 * @return The value of the resource localized for the caller's current culture
	 * settings. If an appropriate resource set exists but  cannot be found, the method
	 * returns .
	 */
	function GetObject(name:String, culture:cs.system.globalization.CultureInfo):Dynamic;
	/**
	 * Retrieves the resource set for a particular culture.
	 * @param culture The culture whose resources are to be retrieved.
	 * @param createIfNotExists to load the resource set, if it has not been loaded
	 * yet; otherwise, .
	 * @param tryParents to use resource fallback to load an appropriate resource if
	 * the resource set cannot be found;  to bypass the resource fallback process.
	 * @return The resource set for the specified culture.
	 */
	function GetResourceSet(culture:cs.system.globalization.CultureInfo, createIfNotExists:Bool, tryParents:Bool):cs.system.resources.ResourceSet;
	@:overload(function(name:String):cs.system.io.UnmanagedMemoryStream {})
	/**
	 * Returns an unmanaged memory stream object from the specified resource.
	 * @param name The name of a resource.
	 * @return An unmanaged memory stream object that represents a resource.
	 */
	function GetStream(name:String, culture:cs.system.globalization.CultureInfo):cs.system.io.UnmanagedMemoryStream;
	@:overload(function(name:String):String {})
	/**
	 * Returns the value of the specified string resource.
	 * @param name The name of the resource to retrieve.
	 * @return The value of the resource localized for the caller's current UI culture,
	 * or  if  cannot be found in a resource set.
	 */
	function GetString(name:String, culture:cs.system.globalization.CultureInfo):String;
	/** Tells the resource manager to call the  method on all  objects and release all resources. */
	function ReleaseAllResources():Void;
}
