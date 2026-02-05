package cs.system.xml.xsl;

/** Provides an interface to a given function defined in the Extensible Stylesheet Language for Transformations (XSLT) style sheet during runtime execution. */
@:native("System.Xml.Xsl.IXsltContextFunction")
extern interface IXsltContextFunction {
	/**
	 * Gets the supplied XML Path Language (XPath) types for the function's argument
	 * list. This information can be used to discover the signature of the function
	 * which allows you to differentiate between overloaded functions.
	 * @return An array of  representing the types for the function's argument list.
	 */
	var ArgTypes(default, never):cs.NativeArray<cs.system.xml.xpath.XPathResultType>;
	/**
	 * Gets the maximum number of arguments for the function. This enables the user to
	 * differentiate between overloaded functions.
	 * @return The maximum number of arguments for the function.
	 */
	var Maxargs(default, never):Int;
	/**
	 * Gets the minimum number of arguments for the function. This enables the user to
	 * differentiate between overloaded functions.
	 * @return The minimum number of arguments for the function.
	 */
	var Minargs(default, never):Int;
	/**
	 * Gets the  representing the XPath type returned by the function.
	 * @return An  representing the XPath type returned by the function
	 */
	var ReturnType(default, never):cs.system.xml.xpath.XPathResultType;
	/**
	 * Provides the method to invoke the function with the given arguments in the given
	 * context.
	 * @param xsltContext The XSLT context for the function call.
	 * @param args The arguments of the function call. Each argument is an element in
	 * the array.
	 * @param docContext The context node for the function call.
	 * @return An  representing the return value of the function.
	 */
	function Invoke(xsltContext:cs.system.xml.xsl.XsltContext, args:cs.NativeArray<Dynamic>, docContext:cs.system.xml.xpath.XPathNavigator):Dynamic;
}
