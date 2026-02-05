package cs.system.xml.schema;

/** An abstract class. Provides information about the included schema. */
@:native("System.Xml.Schema.XmlSchemaExternal")
extern class XmlSchemaExternal extends cs.system.xml.schema.XmlSchemaObject {
	/**
	 * Gets or sets the string id.
	 * @return The string id. The default is . Optional.
	 */
	var Id(default, default):String;
	/**
	 * Gets or sets the  for the referenced schema.
	 * @return The  for the referenced schema.
	 */
	var Schema(default, default):cs.system.xml.schema.XmlSchema;
	/**
	 * Gets or sets the Uniform Resource Identifier (URI) location for the schema,
	 * which tells the schema processor where the schema physically resides.
	 * @return The URI location for the schema. Optional for imported schemas.
	 */
	var SchemaLocation(default, default):String;
	/**
	 * Gets or sets the qualified attributes, which do not belong to the schema target
	 * namespace.
	 * @return Qualified attributes that belong to another target namespace.
	 */
	var UnhandledAttributes(default, default):cs.NativeArray<cs.system.xml.XmlAttribute>;
}
