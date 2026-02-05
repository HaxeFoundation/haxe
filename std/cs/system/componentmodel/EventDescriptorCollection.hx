package cs.system.componentmodel;

/** Represents a collection of  objects. */
@:native("System.ComponentModel.EventDescriptorCollection")
extern class EventDescriptorCollection {
	/** Specifies an empty collection to use, rather than creating a new one with no items. This  field is read-only. */
	static var Empty(default, never):cs.system.componentmodel.EventDescriptorCollection;
	/**
	 * Gets the number of event descriptors in the collection.
	 * @return The number of event descriptors in the collection.
	 */
	var Count(default, never):Int;
	@:overload(function(index0:Int):cs.system.componentmodel.EventDescriptor {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.componentmodel.EventDescriptor;
	@:overload(function(events:cs.NativeArray<cs.system.componentmodel.EventDescriptor>):Void {})
	function new(events:cs.NativeArray<cs.system.componentmodel.EventDescriptor>, readOnly:Bool):Void;
	/**
	 * Adds an  to the end of the collection.
	 * @param value An  to add to the collection.
	 * @return The position of the  within the collection.
	 */
	function Add(value:cs.system.componentmodel.EventDescriptor):Int;
	/** Removes all objects from the collection. */
	function Clear():Void;
	/**
	 * Returns whether the collection contains the given .
	 * @param value The  to find within the collection.
	 * @return if the collection contains the  parameter given; otherwise, .
	 */
	function Contains(value:cs.system.componentmodel.EventDescriptor):Bool;
	/**
	 * Gets the description of the event with the specified name in the collection.
	 * @param name The name of the event to get from the collection.
	 * @param ignoreCase if you want to ignore the case of the event; otherwise, .
	 * @return The  with the specified name, or  if the event does not exist.
	 */
	function Find(name:String, ignoreCase:Bool):cs.system.componentmodel.EventDescriptor;
	/**
	 * Gets an enumerator for this .
	 * @return An enumerator that implements .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Returns the index of the given .
	 * @param value The  to find within the collection.
	 * @return The index of the given  within the collection.
	 */
	function IndexOf(value:cs.system.componentmodel.EventDescriptor):Int;
	/**
	 * Inserts an  to the collection at a specified index.
	 * @param index The index within the collection in which to insert the  parameter.
	 * @param value An  to insert into the collection.
	 */
	function Insert(index:Int, value:cs.system.componentmodel.EventDescriptor):Void;
	/**
	 * Removes the specified  from the collection.
	 * @param value The  to remove from the collection.
	 */
	function Remove(value:cs.system.componentmodel.EventDescriptor):Void;
	/**
	 * Removes the  at the specified index from the collection.
	 * @param index The index of the  to remove.
	 */
	function RemoveAt(index:Int):Void;
	@:overload(function():cs.system.componentmodel.EventDescriptorCollection {})
	@:overload(function(comparer:cs.system.collections.IComparer):cs.system.componentmodel.EventDescriptorCollection {})
	@:overload(function(names:cs.NativeArray<String>):cs.system.componentmodel.EventDescriptorCollection {})
	/**
	 * Sorts the members of this , using the default sort for this collection, which is
	 * usually alphabetical.
	 * @return The new .
	 */
	function Sort(names:cs.NativeArray<String>, comparer:cs.system.collections.IComparer):cs.system.componentmodel.EventDescriptorCollection;
}
