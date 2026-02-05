package cs.system.xml.serialization;

/** Represents certain attributes of a XSD <> element in a WSDL document for generating classes from the document. */
@:native("System.Xml.Serialization.SoapSchemaMember")
extern class SoapSchemaMember {
	/**
	 * Gets or sets a value that corresponds to the name attribute of the WSDL part
	 * element.
	 * @return The element name.
	 */
	var MemberName(default, default):String;
	/**
	 * Gets or sets a value that corresponds to the type attribute of the WSDL part
	 * element.
	 * @return An  that corresponds to the XML type.
	 */
	var MemberType(default, default):cs.system.xml.XmlQualifiedName;
	function new():Void;
}
