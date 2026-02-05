package cs.system.xml.schema;

/** Provides the collections for contained elements in the  class (for example, Attributes, AttributeGroups, Elements, and so on). */
@:native("System.Xml.Schema.XmlSchemaObjectTable")
extern class XmlSchemaObjectTable {
	/**
	 * Gets the number of items contained in the .
	 * @return The number of items contained in the .
	 */
	var Count(default, never):Int;
	/**
	 * Returns a collection of all the named elements in the .
	 * @return A collection of all the named elements in the .
	 */
	var Names(default, never):cs.system.collections.ICollection;
	/**
	 * Returns a collection of all the values for all the elements in the .
	 * @return A collection of all the values for all the elements in the .
	 */
	var Values(default, never):cs.system.collections.ICollection;
	@:native("get_Item")
	function get_Item(index0:cs.system.xml.XmlQualifiedName):cs.system.xml.schema.XmlSchemaObject;
	/**
	 * Determines if the qualified name specified exists in the collection.
	 * @param name The .
	 * @return if the qualified name specified exists in the collection; otherwise, .
	 */
	function Contains(name:cs.system.xml.XmlQualifiedName):Bool;
	/**
	 * Returns an enumerator that can iterate through the .
	 * @return An  that can iterate through .
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
}
