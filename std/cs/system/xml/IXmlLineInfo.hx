package cs.system.xml;

/** Provides an interface to enable a class to return line and position information. */
@:native("System.Xml.IXmlLineInfo")
extern interface IXmlLineInfo {
	/**
	 * Gets the current line number.
	 * @return The current line number or 0 if no line information is available (for
	 * example,  returns ).
	 */
	var LineNumber(default, never):Int;
	/**
	 * Gets the current line position.
	 * @return The current line position or 0 if no line information is available (for
	 * example,  returns ).
	 */
	var LinePosition(default, never):Int;
	/**
	 * Gets a value indicating whether the class can return line information.
	 * @return if  and  can be provided; otherwise, .
	 */
	function HasLineInfo():Bool;
}
