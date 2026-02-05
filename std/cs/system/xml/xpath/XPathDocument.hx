package cs.system.xml.xpath;

/** Provides a fast, read-only, in-memory representation of an XML document by using the XPath data model. */
@:native("System.Xml.XPath.XPathDocument")
extern class XPathDocument {
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(textReader:cs.system.io.TextReader):Void {})
	@:overload(function(uri:String):Void {})
	@:overload(function(reader:cs.system.xml.XmlReader):Void {})
	@:overload(function(uri:String, space:cs.system.xml.XmlSpace):Void {})
	function new(reader:cs.system.xml.XmlReader, space:cs.system.xml.XmlSpace):Void;
	/**
	 * Initializes a read-only  object for navigating through nodes in this .
	 * @return A read-only  object.
	 */
	function CreateNavigator():cs.system.xml.xpath.XPathNavigator;
}
