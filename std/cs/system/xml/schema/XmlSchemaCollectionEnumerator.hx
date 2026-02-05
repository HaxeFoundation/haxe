package cs.system.xml.schema;

/** Supports a simple iteration over a collection. This class cannot be inherited. */
@:native("System.Xml.Schema.XmlSchemaCollectionEnumerator")
extern class XmlSchemaCollectionEnumerator {
	/**
	 * Gets the current  in the collection.
	 * @return The current  in the collection.
	 */
	var Current(default, never):cs.system.xml.schema.XmlSchema;
	/**
	 * Advances the enumerator to the next schema in the collection.
	 * @return if the move was successful;  if the enumerator has passed the end of the
	 * collection.
	 */
	function MoveNext():Bool;
}
