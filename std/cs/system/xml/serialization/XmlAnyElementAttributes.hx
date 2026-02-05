package cs.system.xml.serialization;

/** Represents a collection of  objects. */
@:native("System.Xml.Serialization.XmlAnyElementAttributes")
extern class XmlAnyElementAttributes extends cs.system.collections.CollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.xml.serialization.XmlAnyElementAttribute;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.xml.serialization.XmlAnyElementAttribute):Void;
	function new():Void;
	/**
	 * Adds an  to the collection.
	 * @param attribute The  to add.
	 * @return The index of the newly added .
	 */
	function Add(attribute:cs.system.xml.serialization.XmlAnyElementAttribute):Int;
	/**
	 * Gets a value that indicates whether the specified  exists in the collection.
	 * @param attribute The  you are interested in.
	 * @return if the  exists in the collection; otherwise, .
	 */
	function Contains(attribute:cs.system.xml.serialization.XmlAnyElementAttribute):Bool;
	/**
	 * Copies the entire collection to a compatible one-dimensional array of  objects,
	 * starting at the specified index of the target array.
	 * @param array The one-dimensional array of  objects that is the destination of
	 * the elements copied from the collection. The array must have zero-based
	 * indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.xml.serialization.XmlAnyElementAttribute>, index:Int):Void;
	/**
	 * Gets the index of the specified .
	 * @param attribute The  whose index you want.
	 * @return The index of the specified .
	 */
	function IndexOf(attribute:cs.system.xml.serialization.XmlAnyElementAttribute):Int;
	/**
	 * Inserts an  into the collection at the specified index.
	 * @param index The index where the  is inserted.
	 * @param attribute The  to insert.
	 */
	function Insert(index:Int, attribute:cs.system.xml.serialization.XmlAnyElementAttribute):Void;
	/**
	 * Removes the specified  from the collection.
	 * @param attribute The  to remove.
	 */
	function Remove(attribute:cs.system.xml.serialization.XmlAnyElementAttribute):Void;
}
