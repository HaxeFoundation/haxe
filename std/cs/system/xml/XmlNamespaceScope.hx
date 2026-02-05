package cs.system.xml;

/** Defines the namespace scope. */
@:native("System.Xml.XmlNamespaceScope")
extern enum abstract XmlNamespaceScope(Int) {
	var All = 0;
	var ExcludeXml = 1;
	var Local = 2;
}
