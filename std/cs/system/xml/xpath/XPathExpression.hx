package cs.system.xml.xpath;

/** Provides a typed class that represents a compiled XPath expression. */
@:native("System.Xml.XPath.XPathExpression")
extern class XPathExpression {
	/**
	 * When overridden in a derived class, gets a  representation of the .
	 * @return A  representation of the .
	 */
	var Expression(default, never):String;
	/**
	 * When overridden in a derived class, gets the result type of the XPath
	 * expression.
	 * @return An  value representing the result type of the XPath expression.
	 */
	var ReturnType(default, never):cs.system.xml.xpath.XPathResultType;
	@:overload(function(xpath:String):cs.system.xml.xpath.XPathExpression {})
	/**
	 * Compiles the XPath expression specified and returns an  object representing the
	 * XPath expression.
	 * @param xpath An XPath expression.
	 * @return An  object.
	 */
	static function Compile(xpath:String, nsResolver:cs.system.xml.IXmlNamespaceResolver):cs.system.xml.xpath.XPathExpression;
	@:overload(function(expr:Dynamic, comparer:cs.system.collections.IComparer):Void {})
	/**
	 * When overridden in a derived class, sorts the nodes selected by the XPath
	 * expression according to the specified  object.
	 * @param expr An object representing the sort key. This can be the  value of the
	 * node or an  object with a compiled XPath expression.
	 * @param comparer An  object that provides the specific data type comparisons for
	 * comparing two objects for equivalence.
	 */
	function AddSort(expr:Dynamic, order:cs.system.xml.xpath.XmlSortOrder, caseOrder:cs.system.xml.xpath.XmlCaseOrder, lang:String, dataType:cs.system.xml.xpath.XmlDataType):Void;
	/**
	 * When overridden in a derived class, returns a clone of this .
	 * @return A new  object.
	 */
	function Clone():cs.system.xml.xpath.XPathExpression;
	@:overload(function(nsResolver:cs.system.xml.IXmlNamespaceResolver):Void {})
	/**
	 * When overridden in a derived class, specifies the  object to use for namespace
	 * resolution.
	 * @param nsResolver An object that implements the  interface to use for namespace
	 * resolution.
	 */
	function SetContext(nsManager:cs.system.xml.XmlNamespaceManager):Void;
}
