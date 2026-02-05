package cs.system.xml.schema;

/** Contains a cache of XML Schema definition language (XSD) schemas. */
@:native("System.Xml.Schema.XmlSchemaSet")
extern class XmlSchemaSet {
	/**
	 * Gets or sets the  for the .
	 * @return The  for the . The default is an  instance with the  property set to .
	 */
	var CompilationSettings(default, default):cs.system.xml.schema.XmlSchemaCompilationSettings;
	/**
	 * Gets the number of logical XML Schema definition language (XSD) schemas in the .
	 * @return The number of logical schemas in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets all the global attributes in all the XML Schema definition language (XSD)
	 * schemas in the .
	 * @return The collection of global attributes.
	 */
	var GlobalAttributes(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets all the global elements in all the XML Schema definition language (XSD)
	 * schemas in the .
	 * @return The collection of global elements.
	 */
	var GlobalElements(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets all of the global simple and complex types in all the XML Schema definition
	 * language (XSD) schemas in the .
	 * @return The collection of global simple and complex types.
	 */
	var GlobalTypes(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets a value that indicates whether the XML Schema definition language (XSD)
	 * schemas in the  have been compiled.
	 * @return if the schemas in the  have been compiled since the last time a schema
	 * was added or removed from the ; otherwise, .
	 */
	var IsCompiled(default, never):Bool;
	/**
	 * Gets the default  used by the  when loading new XML Schema definition language
	 * (XSD) schemas.
	 * @return A table of atomized string objects.
	 */
	var NameTable(default, never):cs.system.xml.XmlNameTable;
	/**
	 * Sets the  used to resolve namespaces or locations referenced in include and
	 * import elements of a schema.
	 * @return The  used to resolve namespaces or locations referenced in include and
	 * import elements of a schema.
	 */
	var XmlResolver(never, default):cs.system.xml.XmlResolver;
	@:overload(function():Void {})
	function new(nameTable:cs.system.xml.XmlNameTable):Void;
	@:overload(function(schema:cs.system.xml.schema.XmlSchema):cs.system.xml.schema.XmlSchema {})
	@:overload(function(schemas:cs.system.xml.schema.XmlSchemaSet):Void {})
	@:overload(function(targetNamespace:String, schemaUri:String):cs.system.xml.schema.XmlSchema {})
	/**
	 * Adds the XML Schema definition language (XSD) schema at the URL specified to the
	 * .
	 * @param targetNamespace The schema  property, or  to use the  specified in the
	 * schema.
	 * @param schemaUri The URL that specifies the schema to load.
	 * @return An  object if the schema is valid. If the schema is not valid and a  is
	 * specified, then  is returned and the appropriate validation event is raised.
	 * Otherwise, an  is thrown.
	 */
	function Add(targetNamespace:String, schemaDocument:cs.system.xml.XmlReader):cs.system.xml.schema.XmlSchema;
	/** Compiles the XML Schema definition language (XSD) schemas added to the  into one logical schema. */
	function Compile():Void;
	@:overload(function(targetNamespace:String):Bool {})
	/**
	 * Indicates whether an XML Schema definition language (XSD) schema with the
	 * specified target namespace URI is in the .
	 * @param targetNamespace The schema  property.
	 * @return if a schema with the specified target namespace URI is in the ;
	 * otherwise, .
	 */
	function Contains(schema:cs.system.xml.schema.XmlSchema):Bool;
	/**
	 * Copies all the  objects from the  to the given array, starting at the given
	 * index.
	 * @param schemas The array to copy the objects to.
	 * @param index The index in the array where copying will begin.
	 */
	function CopyTo(schemas:cs.NativeArray<cs.system.xml.schema.XmlSchema>, index:Int):Void;
	/**
	 * Removes the specified XML Schema definition language (XSD) schema from the .
	 * @param schema The  object to remove from the .
	 * @return The  object removed from the  or  if the schema was not found in the .
	 */
	function Remove(schema:cs.system.xml.schema.XmlSchema):cs.system.xml.schema.XmlSchema;
	/**
	 * Removes the specified XML Schema definition language (XSD) schema and all the
	 * schemas it imports from the .
	 * @param schemaToRemove The  object to remove from the .
	 * @return if the  object and all its imports were successfully removed; otherwise,
	 * .
	 */
	function RemoveRecursive(schemaToRemove:cs.system.xml.schema.XmlSchema):Bool;
	/**
	 * Reprocesses an XML Schema definition language (XSD) schema that already exists
	 * in the .
	 * @param schema The schema to reprocess.
	 * @return An  object if the schema is a valid schema. If the schema is not valid
	 * and a  is specified,  is returned and the appropriate validation event is
	 * raised. Otherwise, an  is thrown.
	 */
	function Reprocess(schema:cs.system.xml.schema.XmlSchema):cs.system.xml.schema.XmlSchema;
	@:overload(function():cs.system.collections.ICollection {})
	/**
	 * Returns a collection of all the XML Schema definition language (XSD) schemas in
	 * the .
	 * @return An  object containing all the schemas that have been added to the . If
	 * no schemas have been added to the , an empty  object is returned.
	 */
	function Schemas(targetNamespace:String):cs.system.collections.ICollection;
}
