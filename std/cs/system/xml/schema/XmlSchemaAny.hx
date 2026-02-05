package cs.system.xml.schema;

/** Represents the World Wide Web Consortium (W3C)  element. */
@:native("System.Xml.Schema.XmlSchemaAny")
extern class XmlSchemaAny extends cs.system.xml.schema.XmlSchemaParticle {
	/**
	 * Gets or sets the namespaces containing the elements that can be used.
	 * @return Namespaces for elements that are available for use. The default is .
	 * Optional.
	 */
	var Namespace(default, default):String;
	/**
	 * Gets or sets information about how an application or XML processor should handle
	 * the validation of XML documents for the elements specified by the  element.
	 * @return One of the  values. If no  attribute is specified, the default is .
	 */
	var ProcessContents(default, default):cs.system.xml.schema.XmlSchemaContentProcessing;
	function new():Void;
}
