package cs.system.xml.xsl;

/** Specifies the XSLT features to support during execution of the XSLT style sheet. */
@:native("System.Xml.Xsl.XsltSettings")
extern class XsltSettings {
	/**
	 * Gets an  object with default settings. Support for the XSLT document() function
	 * and embedded script blocks is disabled.
	 * @return An  object with the  and  properties set to .
	 */
	static var Default(default, never):cs.system.xml.xsl.XsltSettings;
	/**
	 * Gets an  object that enables support for the XSLT document() function and
	 * embedded script blocks.
	 * @return An  object with the  and  properties set to .
	 */
	static var TrustedXslt(default, never):cs.system.xml.xsl.XsltSettings;
	/**
	 * Gets or sets a value indicating whether to enable support for the XSLT
	 * document() function.
	 * @return to support the XSLT document() function; otherwise, . The default is .
	 */
	var EnableDocumentFunction(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether to enable support for embedded script
	 * blocks.
	 * @return to support script blocks in XSLT style sheets; otherwise, . The default
	 * is .
	 */
	var EnableScript(default, default):Bool;
	@:overload(function():Void {})
	function new(enableDocumentFunction:Bool, enableScript:Bool):Void;
}
