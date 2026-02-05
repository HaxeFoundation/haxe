package cs.system.xml.schema;

/** Represents the  element from the XML Schema as specified by the World Wide Web Consortium (W3C). Attributes provide additional information for other document elements. The attribute tag is nested between the tags of a document's element for the schema. The XML document displays attributes as named items in the opening tag of an element. */
@:native("System.Xml.Schema.XmlSchemaAttribute")
extern class XmlSchemaAttribute extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets an  object representing the type of the attribute based on the  or  of the
	 * attribute.
	 * @return An  object.
	 */
	var AttributeSchemaType(default, never):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Gets the common language runtime (CLR) object based on the  or  of the attribute
	 * that holds the post-compilation value of the  property.
	 * @return The common runtime library (CLR) object that holds the post-compilation
	 * value of the  property.
	 */
	var AttributeType(default, never):Dynamic;
	/**
	 * Gets or sets the default value for the attribute.
	 * @return The default value for the attribute. The default is a null reference.
	 * Optional.
	 */
	var DefaultValue(default, default):String;
	/**
	 * Gets or sets the fixed value for the attribute.
	 * @return The fixed value for the attribute. The default is null. Optional.
	 */
	var FixedValue(default, default):String;
	/**
	 * Gets or sets the form for the attribute.
	 * @return One of the  values. The default is the value of the  of the schema
	 * element containing the attribute. Optional.
	 */
	var Form(default, default):cs.system.xml.schema.XmlSchemaForm;
	/**
	 * Gets or sets the name of the attribute.
	 * @return The name of the attribute.
	 */
	var Name(default, default):String;
	/**
	 * Gets the qualified name for the attribute.
	 * @return The post-compilation value of the  property.
	 */
	var QualifiedName(default, never):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets the name of an attribute declared in this schema (or another schema
	 * indicated by the specified namespace).
	 * @return The name of the attribute declared.
	 */
	var RefName(default, default):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets the attribute type to a simple type.
	 * @return The simple type defined in this schema.
	 */
	var SchemaType(default, default):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Gets or sets the name of the simple type defined in this schema (or another
	 * schema indicated by the specified namespace).
	 * @return The name of the simple type.
	 */
	var SchemaTypeName(default, default):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets information about how the attribute is used.
	 * @return One of the following values: None, Prohibited, Optional, or Required.
	 * The default is Optional. Optional.
	 */
	var Use(default, default):cs.system.xml.schema.XmlSchemaUse;
	function new():Void;
}
