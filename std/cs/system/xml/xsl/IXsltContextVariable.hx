package cs.system.xml.xsl;

/** Provides an interface to a given variable that is defined in the style sheet during runtime execution. */
@:native("System.Xml.Xsl.IXsltContextVariable")
extern interface IXsltContextVariable {
	/**
	 * Gets a value indicating whether the variable is local.
	 * @return if the variable is a local variable in the current context; otherwise, .
	 */
	var IsLocal(default, never):Bool;
	/**
	 * Gets a value indicating whether the variable is an Extensible Stylesheet
	 * Language Transformations (XSLT) parameter. This can be a parameter to a style
	 * sheet or a template.
	 * @return if the variable is an XSLT parameter; otherwise, .
	 */
	var IsParam(default, never):Bool;
	/**
	 * Gets the  representing the XML Path Language (XPath) type of the variable.
	 * @return The  representing the XPath type of the variable.
	 */
	var VariableType(default, never):cs.system.xml.xpath.XPathResultType;
	/**
	 * Evaluates the variable at runtime and returns an object that represents the
	 * value of the variable.
	 * @param xsltContext An  representing the execution context of the variable.
	 * @return An  representing the value of the variable. Possible return types
	 * include number, string, Boolean, document fragment, or node set.
	 */
	function Evaluate(xsltContext:cs.system.xml.xsl.XsltContext):Dynamic;
}
