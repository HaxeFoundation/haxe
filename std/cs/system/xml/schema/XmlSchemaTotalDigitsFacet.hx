package cs.system.xml.schema;

/** Represents the  facet from XML Schema as specified by the World Wide Web Consortium (W3C). This class can be used to specify a restriction on the number of digits that can be entered for the value of a  element. That value of  must be a positive integer. */
@:native("System.Xml.Schema.XmlSchemaTotalDigitsFacet")
extern class XmlSchemaTotalDigitsFacet extends cs.system.xml.schema.XmlSchemaNumericFacet {
	function new():Void;
}
