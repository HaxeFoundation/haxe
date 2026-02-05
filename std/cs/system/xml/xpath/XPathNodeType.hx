package cs.system.xml.xpath;

/** Defines the XPath node types that can be returned from the  class. */
@:native("System.Xml.XPath.XPathNodeType")
extern enum abstract XPathNodeType(Int) {
	var All = 9;
	var Attribute = 2;
	var Comment = 8;
	var Element = 1;
	var Namespace = 3;
	var ProcessingInstruction = 7;
	var Root = 0;
	var SignificantWhitespace = 5;
	var Text = 4;
	var Whitespace = 6;
}
