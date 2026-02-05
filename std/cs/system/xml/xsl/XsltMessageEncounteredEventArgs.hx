package cs.system.xml.xsl;

/** Provides data for the  event. */
@:native("System.Xml.Xsl.XsltMessageEncounteredEventArgs")
extern class XsltMessageEncounteredEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the contents of the xsl:message element.
	 * @return The contents of the xsl:message element.
	 */
	var Message(default, never):String;
}
