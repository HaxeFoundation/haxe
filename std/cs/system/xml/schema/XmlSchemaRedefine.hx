package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class can be used to allow simple and complex types, groups and attribute groups from external schema files to be redefined in the current schema. This class can also be used to provide versioning for the schema elements. */
@:native("System.Xml.Schema.XmlSchemaRedefine")
extern class XmlSchemaRedefine extends cs.system.xml.schema.XmlSchemaExternal {
	/**
	 * Gets the  , for all attributes in the schema, which holds the post-compilation
	 * value of the  property.
	 * @return The  for all attributes in the schema. The post-compilation value of the
	 * property.
	 */
	var AttributeGroups(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets the , for all groups in the schema, which holds the post-compilation value
	 * of the  property.
	 * @return The  for all groups in the schema. The post-compilation value of the 
	 * property.
	 */
	var Groups(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets the collection of the following classes: , , , , and .
	 * @return The elements contained within the redefine element.
	 */
	var Items(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets the , for all simple and complex types in the schema, which holds the
	 * post-compilation value of the  property.
	 * @return The  for all schema types in the schema. The post-compilation value of
	 * the  property.
	 */
	var SchemaTypes(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	function new():Void;
}
