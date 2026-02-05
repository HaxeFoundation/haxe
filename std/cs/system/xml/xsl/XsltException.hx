package cs.system.xml.xsl;

/** The exception that is thrown when an error occurs while processing an XSLT transformation. */
@:native("System.Xml.Xsl.XsltException")
extern class XsltException extends cs.system.SystemException {
	/**
	 * Gets the line number indicating where the error occurred in the style sheet.
	 * @return The line number indicating where the error occurred in the style sheet.
	 */
	var LineNumber(default, never):Int;
	/**
	 * Gets the line position indicating where the error occurred in the style sheet.
	 * @return The line position indicating where the error occurred in the style
	 * sheet.
	 */
	var LinePosition(default, never):Int;
	/**
	 * Gets the location path of the style sheet.
	 * @return The location path of the style sheet.
	 */
	var SourceUri(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
	/**
	 * Streams all the  properties into the  class for the given .
	 * @param info The  object.
	 * @param context The  object.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
