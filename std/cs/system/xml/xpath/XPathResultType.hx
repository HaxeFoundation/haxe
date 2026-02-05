package cs.system.xml.xpath;

/** Specifies the return type of the XPath expression. */
@:native("System.Xml.XPath.XPathResultType")
extern enum XPathResultType {
	Any;
	Boolean;
	Error;
	Navigator;
	NodeSet;
	Number;
	String;
}
