package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class can be used to specify a restriction on the maximum length of the data value of a  element. The length must be less than the value of the  element. */
@:native("System.Xml.Schema.XmlSchemaMaxLengthFacet")
extern class XmlSchemaMaxLengthFacet extends cs.system.xml.schema.XmlSchemaNumericFacet {
	function new():Void;
}
