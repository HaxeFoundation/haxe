package cs.system.xml.schema;

/** Contains a cache of XML Schema definition language (XSD) and XML-Data Reduced (XDR) schemas. The  class is obsolete. Use  instead. */
@:native("System.Xml.Schema.XmlSchemaCollection")
extern class XmlSchemaCollection {
	/**
	 * Gets the number of namespaces defined in this collection.
	 * @return The number of namespaces defined in this collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets the default  used by the  when loading new schemas.
	 * @return An .
	 */
	var NameTable(default, never):cs.system.xml.XmlNameTable;
	@:native("get_Item")
	function get_Item(index0:String):cs.system.xml.schema.XmlSchema;
	@:overload(function():Void {})
	function new(nametable:cs.system.xml.XmlNameTable):Void;
	@:overload(function(schema:cs.system.xml.schema.XmlSchema):cs.system.xml.schema.XmlSchema {})
	@:overload(function(schema:cs.system.xml.schema.XmlSchemaCollection):Void {})
	@:overload(function(ns:String, uri:String):cs.system.xml.schema.XmlSchema {})
	@:overload(function(ns:String, reader:cs.system.xml.XmlReader):cs.system.xml.schema.XmlSchema {})
	@:overload(function(schema:cs.system.xml.schema.XmlSchema, resolver:cs.system.xml.XmlResolver):cs.system.xml.schema.XmlSchema {})
	/**
	 * Adds the schema located by the given URL into the schema collection.
	 * @param ns The namespace URI associated with the schema. For XML Schemas, this
	 * will typically be the .
	 * @param uri The URL that specifies the schema to load.
	 * @return The  added to the schema collection;  if the schema being added is an
	 * XDR schema or if there are compilation errors in the schema.
	 */
	function Add(ns:String, reader:cs.system.xml.XmlReader, resolver:cs.system.xml.XmlResolver):cs.system.xml.schema.XmlSchema;
	@:overload(function(ns:String):Bool {})
	/**
	 * Gets a value indicating whether a schema with the specified namespace is in the
	 * collection.
	 * @param ns The namespace URI associated with the schema. For XML Schemas, this
	 * will typically be the target namespace.
	 * @return if a schema with the specified namespace is in the collection;
	 * otherwise, .
	 */
	function Contains(schema:cs.system.xml.schema.XmlSchema):Bool;
	/**
	 * Copies all the  objects from this collection into the given array starting at
	 * the given index.
	 * @param array The array to copy the objects to.
	 * @param index The index in  where copying will begin.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.xml.schema.XmlSchema>, index:Int):Void;
	/**
	 * Provides support for the "for each" style iteration over the collection of
	 * schemas.
	 * @return An enumerator for iterating over all schemas in the current collection.
	 */
	function GetEnumerator():cs.system.xml.schema.XmlSchemaCollectionEnumerator;
}
