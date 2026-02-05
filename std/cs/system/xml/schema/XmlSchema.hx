package cs.system.xml.schema;

/** An in-memory representation of an XML Schema, as specified in the World Wide Web Consortium (W3C) XML Schema Part 1: Structures and XML Schema Part 2: Datatypes]. */
@:native("System.Xml.Schema.XmlSchema")
extern class XmlSchema extends cs.system.xml.schema.XmlSchemaObject {
	/** The XML schema instance namespace. This field is constant. */
	static var InstanceNamespace(default, never):String;
	/** The XML schema namespace. This field is constant. */
	static var Namespace(default, never):String;
	/**
	 * Gets or sets the form for attributes declared in the target namespace of the
	 * schema.
	 * @return The  value that indicates if attributes from the target namespace are
	 * required to be qualified with the namespace prefix. The default is .
	 */
	var AttributeFormDefault(default, default):cs.system.xml.schema.XmlSchemaForm;
	/**
	 * Gets the post-schema-compilation value of all the global attribute groups in the
	 * schema.
	 * @return An  collection of all the global attribute groups in the schema.
	 */
	var AttributeGroups(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets the post-schema-compilation value for all the attributes in the schema.
	 * @return An  collection of all the attributes in the schema.
	 */
	var Attributes(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets or sets the  attribute which sets the default value of the  attribute on
	 * element and complex types in the  of the schema.
	 * @return An  value representing the different methods for preventing derivation.
	 * The default value is .
	 */
	var BlockDefault(default, default):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets or sets the form for elements declared in the target namespace of the
	 * schema.
	 * @return The  value that indicates if elements from the target namespace are
	 * required to be qualified with the namespace prefix. The default is .
	 */
	var ElementFormDefault(default, default):cs.system.xml.schema.XmlSchemaForm;
	/**
	 * Gets the post-schema-compilation value for all the elements in the schema.
	 * @return An  collection of all the elements in the schema.
	 */
	var Elements(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets or sets the  attribute which sets the default value of the  attribute on
	 * elements and complex types in the target namespace of the schema.
	 * @return An  value representing the different methods for preventing derivation.
	 * The default value is .
	 */
	var FinalDefault(default, default):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets the post-schema-compilation value of all the groups in the schema.
	 * @return An  collection of all the groups in the schema.
	 */
	var Groups(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets or sets the string ID.
	 * @return The ID of the string. The default value is .
	 */
	var Id(default, default):String;
	/**
	 * Gets the collection of included and imported schemas.
	 * @return An  of the included and imported schemas.
	 */
	var Includes(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Indicates if the schema has been compiled.
	 * @return if schema has been compiled, otherwise, . The default value is .
	 */
	var IsCompiled(default, never):Bool;
	/**
	 * Gets the collection of schema elements in the schema and is used to add new
	 * element types at the  element level.
	 * @return An  of schema elements in the schema.
	 */
	var Items(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets the post-schema-compilation value for all notations in the schema.
	 * @return An  collection of all notations in the schema.
	 */
	var Notations(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets the post-schema-compilation value of all schema types in the schema.
	 * @return An  of all schema types in the schema.
	 */
	var SchemaTypes(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets or sets the Uniform Resource Identifier (URI) of the schema target
	 * namespace.
	 * @return The schema target namespace.
	 */
	var TargetNamespace(default, default):String;
	/**
	 * Gets or sets the qualified attributes which do not belong to the schema target
	 * namespace.
	 * @return An array of qualified  objects that do not belong to the schema target
	 * namespace.
	 */
	var UnhandledAttributes(default, default):cs.NativeArray<cs.system.xml.XmlAttribute>;
	/**
	 * Gets or sets the version of the schema.
	 * @return The version of the schema. The default value is .
	 */
	var Version(default, default):String;
	function new():Void;
	@:overload(function(stream:cs.system.io.Stream, validationEventHandler:cs.system.xml.schema.ValidationEventHandler):cs.system.xml.schema.XmlSchema {})
	@:overload(function(reader:cs.system.io.TextReader, validationEventHandler:cs.system.xml.schema.ValidationEventHandler):cs.system.xml.schema.XmlSchema {})
	/**
	 * Reads an XML Schema  from the supplied stream.
	 * @param stream The supplied data stream.
	 * @param validationEventHandler The validation event handler that receives
	 * information about XML Schema syntax errors.
	 * @return The  object representing the XML Schema.
	 */
	static function Read(reader:cs.system.xml.XmlReader, validationEventHandler:cs.system.xml.schema.ValidationEventHandler):cs.system.xml.schema.XmlSchema;
	@:overload(function(validationEventHandler:cs.system.xml.schema.ValidationEventHandler):Void {})
	/**
	 * Compiles the XML Schema Object Model (SOM) into schema information for
	 * validation. Used to check the syntactic and semantic structure of the
	 * programmatically built SOM. Semantic validation checking is performed during
	 * compilation.
	 * @param validationEventHandler The validation event handler that receives
	 * information about XML Schema validation errors.
	 */
	function Compile(validationEventHandler:cs.system.xml.schema.ValidationEventHandler, resolver:cs.system.xml.XmlResolver):Void;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	@:overload(function(writer:cs.system.xml.XmlWriter):Void {})
	@:overload(function(stream:cs.system.io.Stream, namespaceManager:cs.system.xml.XmlNamespaceManager):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, namespaceManager:cs.system.xml.XmlNamespaceManager):Void {})
	/**
	 * Writes the XML Schema to the supplied data stream.
	 * @param stream The supplied data stream.
	 */
	function Write(writer:cs.system.xml.XmlWriter, namespaceManager:cs.system.xml.XmlNamespaceManager):Void;
}
