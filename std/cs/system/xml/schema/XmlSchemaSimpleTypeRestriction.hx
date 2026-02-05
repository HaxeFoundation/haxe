package cs.system.xml.schema;

/** Represents the  element for simple types from XML Schema as specified by the World Wide Web Consortium (W3C). This class can be used restricting  element. */
@:native("System.Xml.Schema.XmlSchemaSimpleTypeRestriction")
extern class XmlSchemaSimpleTypeRestriction extends cs.system.xml.schema.XmlSchemaSimpleTypeContent {
	/**
	 * Gets or sets information on the base type.
	 * @return The base type for the  element.
	 */
	var BaseType(default, default):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Gets or sets the name of the qualified base type.
	 * @return The qualified name of the simple type restriction base type.
	 */
	var BaseTypeName(default, default):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets an Xml Schema facet.
	 * @return One of the following facet classes: , , , , , , , , , , , .
	 */
	var Facets(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	function new():Void;
}
