package cs.system.xml.xsl;

/** Encapsulates the current execution context of the Extensible Stylesheet Language for Transformations (XSLT) processor allowing XML Path Language (XPath) to resolve functions, parameters, and namespaces within XPath expressions. */
@:native("System.Xml.Xsl.XsltContext")
extern class XsltContext extends cs.system.xml.XmlNamespaceManager {
	/**
	 * When overridden in a derived class, gets a value indicating whether to include
	 * white space nodes in the output.
	 * @return to check white space nodes in the source document for inclusion in the
	 * output;  to not evaluate white space nodes. The default is .
	 */
	var Whitespace(default, never):Bool;
	/**
	 * When overridden in a derived class, compares the base Uniform Resource
	 * Identifiers (URIs) of two documents based upon the order the documents were
	 * loaded by the XSLT processor (that is, the  class).
	 * @param baseUri The base URI of the first document to compare.
	 * @param nextbaseUri The base URI of the second document to compare.
	 * @return An integer value describing the relative order of the two base URIs: -1
	 * if  occurs before ; 0 if the two base URIs are identical; and 1 if  occurs after
	 * .
	 */
	function CompareDocument(baseUri:String, nextbaseUri:String):Int;
	/**
	 * When overridden in a derived class, evaluates whether to preserve white space
	 * nodes or strip them for the given context.
	 * @param node The white space node that is to be preserved or stripped in the
	 * current context.
	 * @return if the white space is to be preserved;  if the white space is to be
	 * stripped.
	 */
	function PreserveWhitespace(node:cs.system.xml.xpath.XPathNavigator):Bool;
	/**
	 * When overridden in a derived class, resolves a function reference and returns an
	 * representing the function. The  is used at execution time to get the return
	 * value of the function.
	 * @param prefix The prefix of the function as it appears in the XPath expression.
	 * @param name The name of the function.
	 * @param ArgTypes An array of argument types for the function being resolved. This
	 * allows you to select between methods with the same name (for example, overloaded
	 * methods).
	 * @return An  representing the function.
	 */
	function ResolveFunction(prefix:String, name:String, ArgTypes:cs.NativeArray<cs.system.xml.xpath.XPathResultType>):cs.system.xml.xsl.IXsltContextFunction;
	/**
	 * When overridden in a derived class, resolves a variable reference and returns an
	 * representing the variable.
	 * @param prefix The prefix of the variable as it appears in the XPath expression.
	 * @param name The name of the variable.
	 * @return An  representing the variable at runtime.
	 */
	function ResolveVariable(prefix:String, name:String):cs.system.xml.xsl.IXsltContextVariable;
}
