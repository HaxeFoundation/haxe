package cs.system.data.sqltypes;

/** Represents XML data stored in or retrieved from a server. */
@:native("System.Data.SqlTypes.SqlXml")
extern class SqlXml {
	/**
	 * Represents a null instance of the  type.
	 * @return A null instance of the  type.
	 */
	static var Null(default, never):cs.system.data.sqltypes.SqlXml;
	/**
	 * Indicates whether this instance represents a null  value.
	 * @return if  is null. Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the string representation of the XML content of this  instance.
	 * @return The string representation of the XML content.
	 */
	var Value(default, never):String;
	@:overload(function():Void {})
	@:overload(function(value:cs.system.io.Stream):Void {})
	function new(value:cs.system.xml.XmlReader):Void;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet An .
	 * @return A string that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets the value of the XML content of this  as a .
	 * @return A -derived instance that contains the XML content. The actual type may
	 * vary (for example, the return value might be ) depending on how the information
	 * is represented internally, on the server.
	 */
	function CreateReader():cs.system.xml.XmlReader;
}
