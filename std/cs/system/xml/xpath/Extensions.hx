package cs.system.xml.xpath;

/** This class contains the LINQ to XML extension methods that enable you to evaluate XPath expressions. */
@:native("System.Xml.XPath.Extensions")
extern class Extensions {
	@:overload(function(node:cs.system.xml.linq.XNode):cs.system.xml.xpath.XPathNavigator {})
	/**
	 * Creates an  for an .
	 * @param node An  that can process XPath queries.
	 * @return An  that can process XPath queries.
	 */
	static function CreateNavigator(node:cs.system.xml.linq.XNode, nameTable:cs.system.xml.XmlNameTable):cs.system.xml.xpath.XPathNavigator;
	@:overload(function(node:cs.system.xml.linq.XNode, expression:String):Dynamic {})
	/**
	 * Evaluates an XPath expression.
	 * @param node The  on which to evaluate the XPath expression.
	 * @param expression A  that contains an XPath expression.
	 * @return An object that can contain a , a , a , or an .
	 */
	static function XPathEvaluate(node:cs.system.xml.linq.XNode, expression:String, resolver:cs.system.xml.IXmlNamespaceResolver):Dynamic;
	@:overload(function(node:cs.system.xml.linq.XNode, expression:String):cs.system.xml.linq.XElement {})
	/**
	 * Selects an  using a XPath expression.
	 * @param node The  on which to evaluate the XPath expression.
	 * @param expression A  that contains an XPath expression.
	 * @return An , or null.
	 */
	static function XPathSelectElement(node:cs.system.xml.linq.XNode, expression:String, resolver:cs.system.xml.IXmlNamespaceResolver):cs.system.xml.linq.XElement;
	@:overload(function(node:cs.system.xml.linq.XNode, expression:String):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement> {})
	/**
	 * Selects a collection of elements using an XPath expression.
	 * @param node The  on which to evaluate the XPath expression.
	 * @param expression A  that contains an XPath expression.
	 * @return An  of  that contains the selected elements.
	 */
	static function XPathSelectElements(node:cs.system.xml.linq.XNode, expression:String, resolver:cs.system.xml.IXmlNamespaceResolver):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XElement>;
}
