package cs.system.resources;

/** Stores all the resources localized for one particular culture, ignoring all other cultures, including any fallback rules. */
@:native("System.Resources.ResourceSet")
extern class ResourceSet {
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(reader:cs.system.resources.IResourceReader):Void {})
	function new(fileName:String):Void;
	/** Closes and releases any resources used by this . */
	function Close():Void;
	/** Disposes of the resources (other than memory) used by the current instance of . */
	function Dispose():Void;
	/**
	 * Returns the preferred resource reader class for this kind of .
	 * @return The  for the preferred resource reader for this kind of .
	 */
	function GetDefaultReader():cs.system.Type;
	/**
	 * Returns the preferred resource writer class for this kind of .
	 * @return The  for the preferred resource writer for this kind of .
	 */
	function GetDefaultWriter():cs.system.Type;
	/**
	 * Returns an  that can iterate through the .
	 * @return An  for this .
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
	@:overload(function(name:String):Dynamic {})
	/**
	 * Searches for a resource object with the specified name.
	 * @param name Case-sensitive name of the resource to search for.
	 * @return The requested resource.
	 */
	function GetObject(name:String, ignoreCase:Bool):Dynamic;
	@:overload(function(name:String):String {})
	/**
	 * Searches for a  resource with the specified name.
	 * @param name Name of the resource to search for.
	 * @return The value of a resource, if the value is a .
	 */
	function GetString(name:String, ignoreCase:Bool):String;
}
