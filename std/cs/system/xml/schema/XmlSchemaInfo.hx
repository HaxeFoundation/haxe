package cs.system.xml.schema;

/** Represents the post-schema-validation infoset of a validated XML node. */
@:native("System.Xml.Schema.XmlSchemaInfo")
extern class XmlSchemaInfo {
	/**
	 * Gets or sets the  object that corresponds to the content type of this validated
	 * XML node.
	 * @return An  object.
	 */
	var ContentType(default, default):cs.system.xml.schema.XmlSchemaContentType;
	/**
	 * Gets or sets a value indicating if this validated XML node was set as the result
	 * of a default being applied during XML Schema Definition Language (XSD) schema
	 * validation.
	 * @return A  value.
	 */
	var IsDefault(default, default):Bool;
	/**
	 * Gets or sets a value indicating if the value for this validated XML node is nil.
	 * @return A  value.
	 */
	var IsNil(default, default):Bool;
	/**
	 * Gets or sets the dynamic schema type for this validated XML node.
	 * @return An  object.
	 */
	var MemberType(default, default):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Gets or sets the compiled  object that corresponds to this validated XML node.
	 * @return An  object.
	 */
	var SchemaAttribute(default, default):cs.system.xml.schema.XmlSchemaAttribute;
	/**
	 * Gets or sets the compiled  object that corresponds to this validated XML node.
	 * @return An  object.
	 */
	var SchemaElement(default, default):cs.system.xml.schema.XmlSchemaElement;
	/**
	 * Gets or sets the static XML Schema Definition Language (XSD) schema type of this
	 * validated XML node.
	 * @return An  object.
	 */
	var SchemaType(default, default):cs.system.xml.schema.XmlSchemaType;
	/**
	 * Gets or sets the  value of this validated XML node.
	 * @return An  value.
	 */
	var Validity(default, default):cs.system.xml.schema.XmlSchemaValidity;
	function new():Void;
}
