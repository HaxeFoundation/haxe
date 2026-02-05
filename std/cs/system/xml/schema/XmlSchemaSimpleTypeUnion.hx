package cs.system.xml.schema;

/** Represents the  element for simple types from XML Schema as specified by the World Wide Web Consortium (W3C). A  datatype can be used to specify the content of a . The value of the  element must be any one of a set of alternative datatypes specified in the union. Union types are always derived types and must comprise at least two alternative datatypes. */
@:native("System.Xml.Schema.XmlSchemaSimpleTypeUnion")
extern class XmlSchemaSimpleTypeUnion extends cs.system.xml.schema.XmlSchemaSimpleTypeContent {
	/**
	 * Gets an array of  objects representing the type of the  element based on the 
	 * and  values of the simple type.
	 * @return An array of  objects representing the type of the  element.
	 */
	var BaseMemberTypes(default, never):cs.NativeArray<cs.system.xml.schema.XmlSchemaSimpleType>;
	/**
	 * Gets the collection of base types.
	 * @return The collection of simple type base values.
	 */
	var BaseTypes(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the array of qualified member names of built-in data types or 
	 * elements defined in this schema (or another schema indicated by the specified
	 * namespace).
	 * @return An array that consists of a list of members of built-in data types or
	 * simple types.
	 */
	var MemberTypes(default, default):cs.NativeArray<cs.system.xml.XmlQualifiedName>;
	function new():Void;
}
