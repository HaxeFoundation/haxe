package cs.system.diagnostics;

/** Provides a strongly typed collection of  objects. */
@:native("System.Diagnostics.ProcessModuleCollection")
extern class ProcessModuleCollection extends cs.system.collections.ReadOnlyCollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.diagnostics.ProcessModule;
	function new(processModules:cs.NativeArray<cs.system.diagnostics.ProcessModule>):Void;
	/**
	 * Determines whether the specified process module exists in the collection.
	 * @param module A  instance that indicates the module to find in this collection.
	 * @return if the module exists in the collection; otherwise, .
	 */
	function Contains(module:cs.system.diagnostics.ProcessModule):Bool;
	/**
	 * Copies an array of  instances to the collection, at the specified index.
	 * @param array An array of  instances to add to the collection.
	 * @param index The location at which to add the new instances.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.diagnostics.ProcessModule>, index:Int):Void;
	/**
	 * Provides the location of a specified module within the collection.
	 * @param module The  whose index is retrieved.
	 * @return The zero-based index that defines the location of the module within the
	 * .
	 */
	function IndexOf(module:cs.system.diagnostics.ProcessModule):Int;
}
