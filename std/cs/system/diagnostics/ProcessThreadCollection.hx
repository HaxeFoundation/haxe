package cs.system.diagnostics;

/** Provides a strongly typed collection of  objects. */
@:native("System.Diagnostics.ProcessThreadCollection")
extern class ProcessThreadCollection extends cs.system.collections.ReadOnlyCollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.diagnostics.ProcessThread;
	function new(processThreads:cs.NativeArray<cs.system.diagnostics.ProcessThread>):Void;
	/**
	 * Appends a process thread to the collection.
	 * @param thread The thread to add to the collection.
	 * @return The zero-based index of the thread in the collection.
	 */
	function Add(thread:cs.system.diagnostics.ProcessThread):Int;
	/**
	 * Determines whether the specified process thread exists in the collection.
	 * @param thread A  instance that indicates the thread to find in this collection.
	 * @return if the thread exists in the collection; otherwise, .
	 */
	function Contains(thread:cs.system.diagnostics.ProcessThread):Bool;
	/**
	 * Copies an array of  instances to the collection, at the specified index.
	 * @param array An array of  instances to add to the collection.
	 * @param index The location at which to add the new instances.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.diagnostics.ProcessThread>, index:Int):Void;
	/**
	 * Provides the location of a specified thread within the collection.
	 * @param thread The  whose index is retrieved.
	 * @return The zero-based index that defines the location of the thread within the
	 * .
	 */
	function IndexOf(thread:cs.system.diagnostics.ProcessThread):Int;
	/**
	 * Inserts a process thread at the specified location in the collection.
	 * @param index The zero-based index indicating the location at which to insert the
	 * thread.
	 * @param thread The thread to insert into the collection.
	 */
	function Insert(index:Int, thread:cs.system.diagnostics.ProcessThread):Void;
	/**
	 * Deletes a process thread from the collection.
	 * @param thread The thread to remove from the collection.
	 */
	function Remove(thread:cs.system.diagnostics.ProcessThread):Void;
}
