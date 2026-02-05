package cs.system.xml.schema;

/** A collection of s. */
@:native("System.Xml.Schema.XmlSchemaObjectCollection")
extern class XmlSchemaObjectCollection extends cs.system.collections.CollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.xml.schema.XmlSchemaObject;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.xml.schema.XmlSchemaObject):Void;
	@:overload(function():Void {})
	function new(parent:cs.system.xml.schema.XmlSchemaObject):Void;
	/**
	 * Adds an  to the .
	 * @param item The .
	 * @return The index at which the item has been added.
	 */
	function Add(item:cs.system.xml.schema.XmlSchemaObject):Int;
	/**
	 * Indicates if the specified  is in the .
	 * @param item The .
	 * @return if the specified qualified name is in the collection; otherwise, returns
	 * . If null is supplied,  is returned because there is no qualified name with a
	 * null name.
	 */
	function Contains(item:cs.system.xml.schema.XmlSchemaObject):Bool;
	/**
	 * Copies all the s from the collection into the given array, starting at the given
	 * index.
	 * @param array The one-dimensional array that is the destination of the elements
	 * copied from the . The array must have zero-based indexing.
	 * @param index The zero-based index in the array at which copying begins.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.xml.schema.XmlSchemaObject>, index:Int):Void;
	/**
	 * Returns an enumerator for iterating through the  contained in the .
	 * @return The iterator returns .
	 */
	function GetEnumerator():cs.system.xml.schema.XmlSchemaObjectEnumerator;
	/**
	 * Gets the collection index corresponding to the specified .
	 * @param item The  whose index you want to return.
	 * @return The index corresponding to the specified .
	 */
	function IndexOf(item:cs.system.xml.schema.XmlSchemaObject):Int;
	/**
	 * Inserts an  to the .
	 * @param index The zero-based index at which an item should be inserted.
	 * @param item The  to insert.
	 */
	function Insert(index:Int, item:cs.system.xml.schema.XmlSchemaObject):Void;
	/**
	 * Removes an  from the .
	 * @param item The  to remove.
	 */
	function Remove(item:cs.system.xml.schema.XmlSchemaObject):Void;
}
