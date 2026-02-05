package cs.system.xml.linq;

/** Represents an XML declaration. */
@:native("System.Xml.Linq.XDeclaration")
extern class XDeclaration {
	/**
	 * Gets or sets the encoding for this document.
	 * @return A  containing the code page name for this document.
	 */
	var Encoding(default, default):String;
	/**
	 * Gets or sets the standalone property for this document.
	 * @return A  containing the standalone property for this document.
	 */
	var Standalone(default, default):String;
	/**
	 * Gets or sets the version property for this document.
	 * @return A  containing the version property for this document.
	 */
	var Version(default, default):String;
	@:overload(function(other:cs.system.xml.linq.XDeclaration):Void {})
	function new(version:String, encoding:String, standalone:String):Void;
	/**
	 * Provides the declaration as a formatted string.
	 * @return A  that contains the formatted XML string.
	 */
	function ToString():String;
}
