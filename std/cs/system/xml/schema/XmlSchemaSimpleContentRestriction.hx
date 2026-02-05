package cs.system.xml.schema;

/** Represents the  element for simple content from XML Schema as specified by the World Wide Web Consortium (W3C). This class can be used to derive simple types by restriction. Such derivations can be used to restrict the range of values for the element to a subset of the values specified in the inherited simple type. */
@:native("System.Xml.Schema.XmlSchemaSimpleContentRestriction")
extern class XmlSchemaSimpleContentRestriction extends cs.system.xml.schema.XmlSchemaContent {
	/**
	 * Gets or sets an  to be used for the attribute value.
	 * @return The  for the attribute value. Optional.
	 */
	var AnyAttribute(default, default):cs.system.xml.schema.XmlSchemaAnyAttribute;
	/**
	 * Gets the  and  collection of attributes for the simple type.
	 * @return The collection of attributes for a simple type.
	 */
	var Attributes(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the simple type base value.
	 * @return The simple type base value.
	 */
	var BaseType(default, default):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Gets or sets the name of the built-in data type or simple type from which this
	 * type is derived.
	 * @return The name of the base type.
	 */
	var BaseTypeName(default, default):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets an Xml Schema facet.
	 * @return One of the following facet classes: , , , , , , , , , , , .
	 */
	var Facets(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	function new():Void;
}
