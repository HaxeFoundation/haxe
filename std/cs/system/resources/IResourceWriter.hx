package cs.system.resources;

/** Provides the base functionality for writing resources to an output file or stream. */
@:native("System.Resources.IResourceWriter")
extern interface IResourceWriter extends cs.system.IDisposable {
	@:overload(function(name:String, value:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(name:String, value:Dynamic):Void {})
	/**
	 * Adds an 8-bit unsigned integer array as a named resource to the list of
	 * resources to be written.
	 * @param name Name of a resource.
	 * @param value Value of a resource as an 8-bit unsigned integer array.
	 */
	function AddResource(name:String, value:String):Void;
	/** Closes the underlying resource file or stream, ensuring all the data has been written to the file. */
	function Close():Void;
	/** Writes all the resources added by the  method to the output file or stream. */
	function Generate():Void;
}
