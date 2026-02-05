package cs.system.xml.xpath;

/** Specifies the data type used to determine sort order. */
@:native("System.Xml.XPath.XmlDataType")
extern enum abstract XmlDataType(Int) {
	var Number = 2;
	var Text = 1;
}
