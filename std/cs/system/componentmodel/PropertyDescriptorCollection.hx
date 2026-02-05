package cs.system.componentmodel;

/** Represents a collection of  objects. */
@:native("System.ComponentModel.PropertyDescriptorCollection")
extern class PropertyDescriptorCollection {
	/** Specifies an empty collection that you can use instead of creating a new one with no items. This  field is read-only. */
	static var Empty(default, never):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Gets the number of property descriptors in the collection.
	 * @return The number of property descriptors in the collection.
	 */
	var Count(default, never):Int;
	@:overload(function(index0:Int):cs.system.componentmodel.PropertyDescriptor {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.componentmodel.PropertyDescriptor;
	@:overload(function(properties:cs.NativeArray<cs.system.componentmodel.PropertyDescriptor>):Void {})
	function new(properties:cs.NativeArray<cs.system.componentmodel.PropertyDescriptor>, readOnly:Bool):Void;
	/**
	 * Adds the specified  to the collection.
	 * @param value The  to add to the collection.
	 * @return The index of the  that was added to the collection.
	 */
	function Add(value:cs.system.componentmodel.PropertyDescriptor):Int;
	/** Removes all  objects from the collection. */
	function Clear():Void;
	/**
	 * Returns whether the collection contains the given .
	 * @param value The  to find in the collection.
	 * @return if the collection contains the given ; otherwise, .
	 */
	function Contains(value:cs.system.componentmodel.PropertyDescriptor):Bool;
	/**
	 * Copies the entire collection to an array, starting at the specified index
	 * number.
	 * @param array An array of  objects to copy elements of the collection to.
	 * @param index The index of the  parameter at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Returns the  with the specified name, using a Boolean to indicate whether to
	 * ignore case.
	 * @param name The name of the  to return from the collection.
	 * @param ignoreCase if you want to ignore the case of the property name;
	 * otherwise, .
	 * @return A  with the specified name, or  if the property does not exist.
	 */
	function Find(name:String, ignoreCase:Bool):cs.system.componentmodel.PropertyDescriptor;
	/**
	 * Returns an enumerator for this class.
	 * @return An enumerator of type .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Returns the index of the given .
	 * @param value The  to return the index of.
	 * @return The index of the given .
	 */
	function IndexOf(value:cs.system.componentmodel.PropertyDescriptor):Int;
	/**
	 * Adds the  to the collection at the specified index number.
	 * @param index The index at which to add the  parameter to the collection.
	 * @param value The  to add to the collection.
	 */
	function Insert(index:Int, value:cs.system.componentmodel.PropertyDescriptor):Void;
	/**
	 * Removes the specified  from the collection.
	 * @param value The  to remove from the collection.
	 */
	function Remove(value:cs.system.componentmodel.PropertyDescriptor):Void;
	/**
	 * Removes the  at the specified index from the collection.
	 * @param index The index of the  to remove from the collection.
	 */
	function RemoveAt(index:Int):Void;
	@:overload(function():cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(comparer:cs.system.collections.IComparer):cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(names:cs.NativeArray<String>):cs.system.componentmodel.PropertyDescriptorCollection {})
	/**
	 * Sorts the members of this collection, using the default sort for this
	 * collection, which is usually alphabetical.
	 * @return A new  that contains the sorted  objects.
	 */
	function Sort(names:cs.NativeArray<String>, comparer:cs.system.collections.IComparer):cs.system.componentmodel.PropertyDescriptorCollection;
}
