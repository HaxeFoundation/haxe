package cs.system.resources;

/** Writes resources in the system-default format to an output file or an output stream. This class cannot be inherited. */
@:native("System.Resources.ResourceWriter")
extern class ResourceWriter {
	/**
	 * Gets or sets a delegate that enables resource assemblies to be written that
	 * target versions of the .NET Framework prior to the .NET Framework 4 by using
	 * qualified assembly names.
	 * @return The type that is encapsulated by the delegate.
	 */
	var TypeNameConverter(default, default):cs.system.Func_2<cs.system.Type, String>;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	function new(fileName:String):Void;
	@:overload(function(name:String, value:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(name:String, value:cs.system.io.Stream):Void {})
	@:overload(function(name:String, value:Dynamic):Void {})
	@:overload(function(name:String, value:String):Void {})
	/**
	 * Adds a named resource specified as a byte array to the list of resources to be
	 * written.
	 * @param name The name of the resource.
	 * @param value Value of the resource as an 8-bit unsigned integer array.
	 */
	function AddResource(name:String, value:cs.system.io.Stream, closeAfterWrite:Bool):Void;
	/**
	 * Adds a unit of data as a resource to the list of resources to be written.
	 * @param name A name that identifies the resource that contains the added data.
	 * @param typeName The type name of the added data.
	 * @param serializedData A byte array that contains the binary representation of
	 * the added data.
	 */
	function AddResourceData(name:String, typeName:String, serializedData:cs.NativeArray<cs.UInt8>):Void;
	/** Saves the resources to the output stream and then closes it. */
	function Close():Void;
	/** Allows users to close the resource file or stream, explicitly releasing resources. */
	function Dispose():Void;
	/** Saves all resources to the output stream in the system default format. */
	function Generate():Void;
}
