package cs.system.xml.xpath;

/** Specifies the sort order for uppercase and lowercase letters. */
@:native("System.Xml.XPath.XmlCaseOrder")
extern enum abstract XmlCaseOrder(Int) {
	var LowerFirst = 2;
	var None = 0;
	var UpperFirst = 1;
}
