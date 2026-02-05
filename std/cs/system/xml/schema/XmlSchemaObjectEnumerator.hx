package cs.system.xml.schema;

/** Represents the enumerator for the . */
@:native("System.Xml.Schema.XmlSchemaObjectEnumerator")
extern class XmlSchemaObjectEnumerator {
	/**
	 * Gets the current  in the collection.
	 * @return The current .
	 */
	var Current(default, never):cs.system.xml.schema.XmlSchemaObject;
	/**
	 * Moves to the next item in the collection.
	 * @return at the end of the collection.
	 */
	function MoveNext():Bool;
	/** Resets the enumerator to the start of the collection. */
	function Reset():Void;
}
