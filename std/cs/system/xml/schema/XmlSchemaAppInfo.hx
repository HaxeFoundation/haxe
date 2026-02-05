package cs.system.xml.schema;

/** Represents the World Wide Web Consortium (W3C)  element. */
@:native("System.Xml.Schema.XmlSchemaAppInfo")
extern class XmlSchemaAppInfo extends cs.system.xml.schema.XmlSchemaObject {
	/**
	 * Gets or sets an array of  objects that represents the  child nodes.
	 * @return An array of  objects that represents the  child nodes.
	 */
	var Markup(default, default):cs.NativeArray<cs.system.xml.XmlNode>;
	/**
	 * Gets or sets the source of the application information.
	 * @return A Uniform Resource Identifier (URI) reference. The default is .
	 * Optional.
	 */
	var Source(default, default):String;
	function new():Void;
}
