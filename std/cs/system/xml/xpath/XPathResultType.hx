package cs.system.xml.xpath;

/** Specifies the return type of the XPath expression. */
@:native("System.Xml.XPath.XPathResultType")
extern enum abstract XPathResultType(Int) {
	var Any = 5;
	var Boolean = 2;
	var Error = 6;
	var Navigator = 1;
	var NodeSet = 3;
	var Number = 0;
	var String = 1;
}
