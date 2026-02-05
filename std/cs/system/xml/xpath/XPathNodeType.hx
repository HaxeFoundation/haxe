package cs.system.xml.xpath;

/** Defines the XPath node types that can be returned from the  class. */
@:native("System.Xml.XPath.XPathNodeType")
extern enum XPathNodeType {
	All;
	Attribute;
	Comment;
	Element;
	Namespace;
	ProcessingInstruction;
	Root;
	SignificantWhitespace;
	Text;
	Whitespace;
}
