package cs.system.xml.schema;

/** Represents the  element for simple content from XML Schema as specified by the World Wide Web Consortium (W3C). This class can be used to derive simple types by extension. Such derivations are used to extend the simple type content of the element by adding attributes. */
@:native("System.Xml.Schema.XmlSchemaSimpleContentExtension")
extern class XmlSchemaSimpleContentExtension extends cs.system.xml.schema.XmlSchemaContent {
	/**
	 * Gets or sets the  to be used for the attribute value.
	 * @return The . Optional.
	 */
	var AnyAttribute(default, default):cs.system.xml.schema.XmlSchemaAnyAttribute;
	/**
	 * Gets the collection of  and .
	 * @return The collection of attributes for the  element.
	 */
	var Attributes(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the name of a built-in data type or simple type from which this
	 * type is extended.
	 * @return The base type name.
	 */
	var BaseTypeName(default, default):cs.system.xml.XmlQualifiedName;
	function new():Void;
}
