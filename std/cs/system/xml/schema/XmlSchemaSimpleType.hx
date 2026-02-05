package cs.system.xml.schema;

/** Represents the  element for simple content from XML Schema as specified by the World Wide Web Consortium (W3C). This class defines a simple type. Simple types can specify information and constraints for the value of attributes or elements with text-only content. */
@:native("System.Xml.Schema.XmlSchemaSimpleType")
extern class XmlSchemaSimpleType extends cs.system.xml.schema.XmlSchemaType {
	/**
	 * Gets or sets one of , , or .
	 * @return One of , , or .
	 */
	var Content(default, default):cs.system.xml.schema.XmlSchemaSimpleTypeContent;
	function new():Void;
}
