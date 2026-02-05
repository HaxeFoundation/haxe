package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class specifies information to be read or used by humans within an . */
@:native("System.Xml.Schema.XmlSchemaDocumentation")
extern class XmlSchemaDocumentation extends cs.system.xml.schema.XmlSchemaObject {
	/**
	 * Gets or sets the  attribute. This serves as an indicator of the language used in
	 * the contents.
	 * @return The  attribute. Optional.
	 */
	var Language(default, default):String;
	/**
	 * Gets or sets an array of  that represents the documentation child nodes.
	 * @return The array that represents the documentation child nodes.
	 */
	var Markup(default, default):cs.NativeArray<cs.system.xml.XmlNode>;
	/**
	 * Gets or sets the Uniform Resource Identifier (URI) source of the information.
	 * @return A URI reference. The default is . Optional.
	 */
	var Source(default, default):String;
	function new():Void;
}
