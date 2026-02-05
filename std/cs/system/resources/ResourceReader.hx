package cs.system.resources;

/** Enumerates the resources in a binary resources (.resources) file by reading sequential resource name/value pairs. */
@:native("System.Resources.ResourceReader")
extern class ResourceReader {
	@:overload(function(stream:cs.system.io.Stream):Void {})
	function new(fileName:String):Void;
	/** Releases all operating system resources associated with this  object. */
	function Close():Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Returns an enumerator for this  object.
	 * @return An enumerator for this  object.
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
	/**
	 * Retrieves the type name and data of a named resource from an open resource file
	 * or stream.
	 * @param resourceName The name of a resource.
	 * @param resourceType When this method returns, contains a string that represents
	 * the type name of the retrieved resource. This parameter is passed uninitialized.
	 * @param resourceData When this method returns, contains a byte array that is the
	 * binary representation of the retrieved type. This parameter is passed
	 * uninitialized.
	 */
	function GetResourceData(resourceName:String, resourceType:cs.Ref<String>, resourceData:cs.Ref<cs.NativeArray<cs.UInt8>>):Void;
}
