package cs.system.xml.schema;

/** Represents the World Wide Web Consortium (W3C)  element. */
@:native("System.Xml.Schema.XmlSchemaAnnotation")
extern class XmlSchemaAnnotation extends cs.system.xml.schema.XmlSchemaObject {
	/**
	 * Gets or sets the string id.
	 * @return The string id. The default is . Optional.
	 */
	var Id(default, default):String;
	/**
	 * Gets the  collection that is used to store the  and  child elements.
	 * @return An  of  and  child elements.
	 */
	var Items(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the qualified attributes that do not belong to the schema's target
	 * namespace.
	 * @return An array of  objects that do not belong to the schema's target
	 * namespace.
	 */
	var UnhandledAttributes(default, default):cs.NativeArray<cs.system.xml.XmlAttribute>;
	function new():Void;
}
