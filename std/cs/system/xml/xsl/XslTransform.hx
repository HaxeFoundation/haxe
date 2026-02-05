package cs.system.xml.xsl;

/** Transforms XML data using an Extensible Stylesheet Language for Transformations (XSLT) style sheet. */
@:native("System.Xml.Xsl.XslTransform")
extern class XslTransform {
	/**
	 * Sets the  used to resolve external resources when the  method is called.
	 * @return The  to use during transformation. If set to , the XSLT document()
	 * function is not resolved.
	 */
	var XmlResolver(never, default):cs.system.xml.XmlResolver;
	function new():Void;
	@:overload(function(url:String):Void {})
	@:overload(function(stylesheet:cs.system.xml.XmlReader):Void {})
	@:overload(function(stylesheet:cs.system.xml.xpath.IXPathNavigable):Void {})
	@:overload(function(stylesheet:cs.system.xml.xpath.XPathNavigator):Void {})
	@:overload(function(url:String, resolver:cs.system.xml.XmlResolver):Void {})
	@:overload(function(stylesheet:cs.system.xml.XmlReader, resolver:cs.system.xml.XmlResolver):Void {})
	@:overload(function(stylesheet:cs.system.xml.xpath.IXPathNavigable, resolver:cs.system.xml.XmlResolver):Void {})
	/**
	 * Loads the XSLT style sheet specified by a URL.
	 * @param url The URL that specifies the XSLT style sheet to load.
	 */
	function Load(stylesheet:cs.system.xml.xpath.XPathNavigator, resolver:cs.system.xml.XmlResolver):Void;
	@:overload(function(inputfile:String, outputfile:String):Void {})
	@:overload(function(input:cs.system.xml.xpath.IXPathNavigable, args:cs.system.xml.xsl.XsltArgumentList):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.xml.xpath.XPathNavigator, args:cs.system.xml.xsl.XsltArgumentList):cs.system.xml.XmlReader {})
	@:overload(function(inputfile:String, outputfile:String, resolver:cs.system.xml.XmlResolver):Void {})
	@:overload(function(input:cs.system.xml.xpath.IXPathNavigable, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.io.Stream):Void {})
	@:overload(function(input:cs.system.xml.xpath.IXPathNavigable, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.io.TextWriter):Void {})
	@:overload(function(input:cs.system.xml.xpath.IXPathNavigable, args:cs.system.xml.xsl.XsltArgumentList, resolver:cs.system.xml.XmlResolver):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.xml.xpath.IXPathNavigable, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.xml.XmlWriter):Void {})
	@:overload(function(input:cs.system.xml.xpath.XPathNavigator, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.io.Stream):Void {})
	@:overload(function(input:cs.system.xml.xpath.XPathNavigator, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.io.TextWriter):Void {})
	@:overload(function(input:cs.system.xml.xpath.XPathNavigator, args:cs.system.xml.xsl.XsltArgumentList, resolver:cs.system.xml.XmlResolver):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.xml.xpath.XPathNavigator, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.xml.XmlWriter):Void {})
	@:overload(function(input:cs.system.xml.xpath.IXPathNavigable, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.io.Stream, resolver:cs.system.xml.XmlResolver):Void {})
	@:overload(function(input:cs.system.xml.xpath.IXPathNavigable, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.io.TextWriter, resolver:cs.system.xml.XmlResolver):Void {})
	@:overload(function(input:cs.system.xml.xpath.IXPathNavigable, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.xml.XmlWriter, resolver:cs.system.xml.XmlResolver):Void {})
	@:overload(function(input:cs.system.xml.xpath.XPathNavigator, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.io.Stream, resolver:cs.system.xml.XmlResolver):Void {})
	@:overload(function(input:cs.system.xml.xpath.XPathNavigator, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.io.TextWriter, resolver:cs.system.xml.XmlResolver):Void {})
	/**
	 * Transforms the XML data in the input file and outputs the result to an output
	 * file.
	 * @param inputfile The URL of the source document to be transformed.
	 * @param outputfile The URL of the output file.
	 */
	function Transform(input:cs.system.xml.xpath.XPathNavigator, args:cs.system.xml.xsl.XsltArgumentList, output:cs.system.xml.XmlWriter, resolver:cs.system.xml.XmlResolver):Void;
}
