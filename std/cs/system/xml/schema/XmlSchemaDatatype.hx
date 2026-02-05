package cs.system.xml.schema;

/** The  class is an abstract class for mapping XML Schema definition language (XSD) types to Common Language Runtime (CLR) types. */
@:native("System.Xml.Schema.XmlSchemaDatatype")
extern class XmlSchemaDatatype {
	/**
	 * When overridden in a derived class, gets the type for the  as specified in the
	 * World Wide Web Consortium (W3C) XML 1.0 specification.
	 * @return An  value for the .
	 */
	var TokenizedType(default, never):cs.system.xml.XmlTokenizedType;
	/**
	 * Gets the  value for the simple type.
	 * @return The  value for the simple type.
	 */
	var TypeCode(default, never):cs.system.xml.schema.XmlTypeCode;
	/**
	 * When overridden in a derived class, gets the Common Language Runtime (CLR) type
	 * of the item.
	 * @return The Common Language Runtime (CLR) type of the item.
	 */
	var ValueType(default, never):cs.system.Type;
	/**
	 * Gets the  value for the simple type.
	 * @return The  value for the simple type.
	 */
	var Variety(default, never):cs.system.xml.schema.XmlSchemaDatatypeVariety;
	@:overload(function(value:Dynamic, targetType:cs.system.Type):Dynamic {})
	/**
	 * Converts the value specified, whose type is one of the valid Common Language
	 * Runtime (CLR) representations of the XML schema type represented by the , to the
	 * CLR type specified.
	 * @param value The input value to convert to the specified type.
	 * @param targetType The target type to convert the input value to.
	 * @return The converted input value.
	 */
	function ChangeType(value:Dynamic, targetType:cs.system.Type, namespaceResolver:cs.system.xml.IXmlNamespaceResolver):Dynamic;
	/**
	 * The  method always returns .
	 * @param datatype The .
	 * @return Always returns .
	 */
	function IsDerivedFrom(datatype:cs.system.xml.schema.XmlSchemaDatatype):Bool;
	/**
	 * When overridden in a derived class, validates the  specified against a built-in
	 * or user-defined simple type.
	 * @param s The  to validate against the simple type.
	 * @param nameTable The  to use for atomization while parsing the  if this  object
	 * represents the xs:NCName type.
	 * @param nsmgr The  object to use while parsing the  if this  object represents
	 * the xs:QName type.
	 * @return An  that can be cast safely to the type returned by the  property.
	 */
	function ParseValue(s:String, nameTable:cs.system.xml.XmlNameTable, nsmgr:cs.system.xml.IXmlNamespaceResolver):Dynamic;
}
