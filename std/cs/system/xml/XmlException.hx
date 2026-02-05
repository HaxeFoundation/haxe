package cs.system.xml;

/** Returns detailed information about the last exception. */
@:native("System.Xml.XmlException")
extern class XmlException extends cs.system.SystemException {
	/**
	 * Gets the line number indicating where the error occurred.
	 * @return The line number indicating where the error occurred.
	 */
	var LineNumber(default, never):Int;
	/**
	 * Gets the line position indicating where the error occurred.
	 * @return The line position indicating where the error occurred.
	 */
	var LinePosition(default, never):Int;
	/**
	 * Gets the location of the XML file.
	 * @return The source URI for the XML data. If there is no source URI, this
	 * property returns .
	 */
	var SourceUri(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, innerException:cs.system.Exception, lineNumber:Int, linePosition:Int):Void;
	/**
	 * Streams all the  properties into the  class for the given .
	 * @param info The  object.
	 * @param context The  object.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
