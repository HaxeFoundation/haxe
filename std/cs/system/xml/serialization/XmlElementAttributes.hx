package cs.system.xml.serialization;

/** Represents a collection of  objects used by the  to override the default way it serializes a class. */
@:native("System.Xml.Serialization.XmlElementAttributes")
extern class XmlElementAttributes extends cs.system.collections.CollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.xml.serialization.XmlElementAttribute;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.xml.serialization.XmlElementAttribute):Void;
	function new():Void;
	/**
	 * Adds an  to the collection.
	 * @param attribute The  to add.
	 * @return The zero-based index of the newly added item.
	 */
	function Add(attribute:cs.system.xml.serialization.XmlElementAttribute):Int;
	/**
	 * Determines whether the collection contains the specified object.
	 * @param attribute The  to look for.
	 * @return if the object exists in the collection; otherwise, .
	 */
	function Contains(attribute:cs.system.xml.serialization.XmlElementAttribute):Bool;
	/**
	 * Copies the , or a portion of it to a one-dimensional array.
	 * @param array The  array to hold the copied elements.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.xml.serialization.XmlElementAttribute>, index:Int):Void;
	/**
	 * Gets the index of the specified .
	 * @param attribute The  whose index is being retrieved.
	 * @return The zero-based index of the .
	 */
	function IndexOf(attribute:cs.system.xml.serialization.XmlElementAttribute):Int;
	/**
	 * Inserts an  into the collection.
	 * @param index The zero-based index where the member is inserted.
	 * @param attribute The  to insert.
	 */
	function Insert(index:Int, attribute:cs.system.xml.serialization.XmlElementAttribute):Void;
	/**
	 * Removes the specified object from the collection.
	 * @param attribute The  to remove from the collection.
	 */
	function Remove(attribute:cs.system.xml.serialization.XmlElementAttribute):Void;
}
