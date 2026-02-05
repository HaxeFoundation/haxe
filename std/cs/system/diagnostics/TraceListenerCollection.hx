package cs.system.diagnostics;

/** Provides a thread-safe list of  objects. */
@:native("System.Diagnostics.TraceListenerCollection")
extern class TraceListenerCollection {
	/**
	 * Gets the number of listeners in the list.
	 * @return The number of listeners in the list.
	 */
	var Count(default, never):Int;
	@:overload(function(index0:Int):cs.system.diagnostics.TraceListener {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.diagnostics.TraceListener;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.diagnostics.TraceListener):Void;
	/**
	 * Adds a  to the list.
	 * @param listener A  to add to the list.
	 * @return The position at which the new listener was inserted.
	 */
	function Add(listener:cs.system.diagnostics.TraceListener):Int;
	@:overload(function(value:cs.system.diagnostics.TraceListenerCollection):Void {})
	/**
	 * Adds an array of  objects to the list.
	 * @param value An array of  objects to add to the list.
	 */
	function AddRange(value:cs.NativeArray<cs.system.diagnostics.TraceListener>):Void;
	/** Clears all the listeners from the list. */
	function Clear():Void;
	/**
	 * Checks whether the list contains the specified listener.
	 * @param listener A  to find in the list.
	 * @return if the listener is in the list; otherwise, .
	 */
	function Contains(listener:cs.system.diagnostics.TraceListener):Bool;
	/**
	 * Copies a section of the current  list to the specified array at the specified
	 * index.
	 * @param listeners An array of type  to copy the elements into.
	 * @param index The starting index number in the current list to copy from.
	 */
	function CopyTo(listeners:cs.NativeArray<cs.system.diagnostics.TraceListener>, index:Int):Void;
	/**
	 * Gets an enumerator for this list.
	 * @return An enumerator of type .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Gets the index of the specified listener.
	 * @param listener A  to find in the list.
	 * @return The index of the listener, if it can be found in the list; otherwise,
	 * -1.
	 */
	function IndexOf(listener:cs.system.diagnostics.TraceListener):Int;
	/**
	 * Inserts the listener at the specified index.
	 * @param index The position in the list to insert the new .
	 * @param listener A  to insert in the list.
	 */
	function Insert(index:Int, listener:cs.system.diagnostics.TraceListener):Void;
	@:overload(function(listener:cs.system.diagnostics.TraceListener):Void {})
	/**
	 * Removes from the collection the specified .
	 * @param listener A  to remove from the list.
	 */
	function Remove(name:String):Void;
	/**
	 * Removes from the collection the  at the specified index.
	 * @param index The zero-based index of the  to remove from the list.
	 */
	function RemoveAt(index:Int):Void;
}
