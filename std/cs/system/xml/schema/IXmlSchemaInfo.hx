package cs.system.xml.schema;

/** Defines the post-schema-validation infoset of a validated XML node. */
@:native("System.Xml.Schema.IXmlSchemaInfo")
extern interface IXmlSchemaInfo {
	/**
	 * Gets a value indicating if this validated XML node was set as the result of a
	 * default being applied during XML Schema Definition Language (XSD) schema
	 * validation.
	 * @return if this validated XML node was set as the result of a default being
	 * applied during schema validation; otherwise, .
	 */
	var IsDefault(default, never):Bool;
	/**
	 * Gets a value indicating if the value for this validated XML node is nil.
	 * @return if the value for this validated XML node is nil; otherwise, .
	 */
	var IsNil(default, never):Bool;
	/**
	 * Gets the dynamic schema type for this validated XML node.
	 * @return An  object that represents the dynamic schema type for this validated
	 * XML node.
	 */
	var MemberType(default, never):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Gets the compiled  that corresponds to this validated XML node.
	 * @return An  that corresponds to this validated XML node.
	 */
	var SchemaAttribute(default, never):cs.system.xml.schema.XmlSchemaAttribute;
	/**
	 * Gets the compiled  that corresponds to this validated XML node.
	 * @return An  that corresponds to this validated XML node.
	 */
	var SchemaElement(default, never):cs.system.xml.schema.XmlSchemaElement;
	/**
	 * Gets the static XML Schema Definition Language (XSD) schema type of this
	 * validated XML node.
	 * @return An  of this validated XML node.
	 */
	var SchemaType(default, never):cs.system.xml.schema.XmlSchemaType;
	/**
	 * Gets the  value of this validated XML node.
	 * @return An  value of this validated XML node.
	 */
	var Validity(default, never):cs.system.xml.schema.XmlSchemaValidity;
}
