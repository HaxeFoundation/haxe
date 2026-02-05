package cs.system.xml.serialization;

/** Represents a collection of  objects. */
@:native("System.Xml.Serialization.XmlArrayItemAttributes")
extern class XmlArrayItemAttributes extends cs.system.collections.CollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.xml.serialization.XmlArrayItemAttribute;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.xml.serialization.XmlArrayItemAttribute):Void;
	function new():Void;
	/**
	 * Adds an  to the collection.
	 * @param attribute The  to add to the collection.
	 * @return The index of the added item.
	 */
	function Add(attribute:cs.system.xml.serialization.XmlArrayItemAttribute):Int;
	/**
	 * Determines whether the collection contains the specified .
	 * @param attribute The  to check for.
	 * @return if the collection contains the specified ; otherwise, .
	 */
	function Contains(attribute:cs.system.xml.serialization.XmlArrayItemAttribute):Bool;
	/**
	 * Copies an  array to the collection, starting at a specified target index.
	 * @param array The array of  objects to copy to the collection.
	 * @param index The index at which the copied attributes begin.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.xml.serialization.XmlArrayItemAttribute>, index:Int):Void;
	/**
	 * Returns the zero-based index of the first occurrence of the specified  in the
	 * collection or -1 if the attribute is not found in the collection.
	 * @param attribute The  to locate in the collection.
	 * @return The first index of the  in the collection or -1 if the attribute is not
	 * found in the collection.
	 */
	function IndexOf(attribute:cs.system.xml.serialization.XmlArrayItemAttribute):Int;
	/**
	 * Inserts an  into the collection at the specified index.
	 * @param index The index at which the attribute is inserted.
	 * @param attribute The  to insert.
	 */
	function Insert(index:Int, attribute:cs.system.xml.serialization.XmlArrayItemAttribute):Void;
	/**
	 * Removes an  from the collection, if it is present.
	 * @param attribute The  to remove.
	 */
	function Remove(attribute:cs.system.xml.serialization.XmlArrayItemAttribute):Void;
}
