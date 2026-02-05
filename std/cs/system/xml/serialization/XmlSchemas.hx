package cs.system.xml.serialization;

/** Represents the collection of XML schemas. */
@:native("System.Xml.Serialization.XmlSchemas")
extern class XmlSchemas extends cs.system.collections.CollectionBase {
	/**
	 * Gets a value that indicates whether the schemas have been compiled.
	 * @return , if the schemas have been compiled; otherwise, .
	 */
	var IsCompiled(default, never):Bool;
	@:overload(function(index0:Int):cs.system.xml.schema.XmlSchema {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.xml.schema.XmlSchema;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.xml.schema.XmlSchema):Void;
	function new():Void;
	/**
	 * Static method that determines whether the specified XML schema contains a custom
	 * attribute set to , or its equivalent.
	 * @param schema The XML schema to check for an  attribute with a  value.
	 * @return if the specified schema exists; otherwise, .
	 */
	static function IsDataSet(schema:cs.system.xml.schema.XmlSchema):Bool;
	@:overload(function(schema:cs.system.xml.schema.XmlSchema):Int {})
	@:overload(function(schemas:cs.system.xml.serialization.XmlSchemas):Void {})
	/**
	 * Adds an object to the end of the collection.
	 * @param schema The  object to be added to the collection of objects.
	 * @return The index at which the  is added.
	 */
	function Add(schema:cs.system.xml.schema.XmlSchema, baseUri:cs.system.Uri):Int;
	/**
	 * Adds an  object that represents an assembly reference to the collection.
	 * @param schema The  to add.
	 */
	function AddReference(schema:cs.system.xml.schema.XmlSchema):Void;
	/**
	 * Processes the element and attribute names in the XML schemas and, optionally,
	 * validates the XML schemas.
	 * @param handler A  that specifies the callback method that handles errors and
	 * warnings during XML Schema validation, if the strict parameter is set to .
	 * @param fullCompile to validate the XML schemas in the collection using the 
	 * method of the  class; otherwise, .
	 */
	function Compile(handler:cs.system.xml.schema.ValidationEventHandler, fullCompile:Bool):Void;
	@:overload(function(targetNamespace:String):Bool {})
	/**
	 * Returns a value that indicates whether the collection contains an  object that
	 * belongs to the specified namespace.
	 * @param targetNamespace The namespace of the item to check for.
	 * @return if the item is found; otherwise, .
	 */
	function Contains(schema:cs.system.xml.schema.XmlSchema):Bool;
	/**
	 * Copies the entire  to a compatible one-dimensional , which starts at the
	 * specified index of the target array.
	 * @param array The one-dimensional  that is the destination of the schemas copied
	 * from . The  must have zero-based indexing.
	 * @param index A 32-bit integer that represents the index in the array where
	 * copying begins.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.xml.schema.XmlSchema>, index:Int):Void;
	/**
	 * Locates in one of the XML schemas an  of the specified name and type.
	 * @param name An  that specifies a fully qualified name with a namespace used to
	 * locate an  object in the collection.
	 * @param type The  of the object to find. Possible types include: , , , , and .
	 * @return An  instance, such as an  or .
	 */
	function Find(name:cs.system.xml.XmlQualifiedName, type:cs.system.Type):Dynamic;
	/**
	 * Gets a collection of schemas that belong to the same namespace.
	 * @param ns The namespace of the schemas to retrieve.
	 * @return An  implementation that contains the schemas.
	 */
	function GetSchemas(ns:String):cs.system.collections.IList;
	/**
	 * Searches for the specified schema and returns the zero-based index of the first
	 * occurrence within the entire .
	 * @param schema The  to locate.
	 * @return The zero-based index of the first occurrence of the value within the
	 * entire , if found; otherwise, -1.
	 */
	function IndexOf(schema:cs.system.xml.schema.XmlSchema):Int;
	/**
	 * Inserts a schema into the  at the specified index.
	 * @param index The zero-based index at which  should be inserted.
	 * @param schema The  object to be inserted.
	 */
	function Insert(index:Int, schema:cs.system.xml.schema.XmlSchema):Void;
	/**
	 * Removes the first occurrence of a specific schema from the .
	 * @param schema The  to remove.
	 */
	function Remove(schema:cs.system.xml.schema.XmlSchema):Void;
}
