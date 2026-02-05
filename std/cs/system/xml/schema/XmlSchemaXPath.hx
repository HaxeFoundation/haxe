package cs.system.xml.schema;

/** Represents the World Wide Web Consortium (W3C)  element. */
@:native("System.Xml.Schema.XmlSchemaXPath")
extern class XmlSchemaXPath extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets or sets the attribute for the XPath expression.
	 * @return The string attribute value for the XPath expression.
	 */
	var XPath(default, default):String;
	function new():Void;
}
