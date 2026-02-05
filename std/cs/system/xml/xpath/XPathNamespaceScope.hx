package cs.system.xml.xpath;

/** Defines the namespace scope. */
@:native("System.Xml.XPath.XPathNamespaceScope")
extern enum abstract XPathNamespaceScope(Int) {
	var All = 0;
	var ExcludeXml = 1;
	var Local = 2;
}
