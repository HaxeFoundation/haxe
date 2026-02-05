package cs.system.xml.serialization;

/** Enables iteration over a collection of  objects. */
@:native("System.Xml.Serialization.XmlSchemaEnumerator")
extern class XmlSchemaEnumerator {
	/**
	 * Gets the current element in the collection.
	 * @return The current  object in the collection.
	 */
	var Current(default, never):cs.system.xml.schema.XmlSchema;
	function new(list:cs.system.xml.serialization.XmlSchemas):Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
	/**
	 * Advances the enumerator to the next item in the collection.
	 * @return if the move is successful; otherwise, .
	 */
	function MoveNext():Bool;
}
