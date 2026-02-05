package cs.system.xml.xpath;

/** Provides the exception thrown when an error occurs while processing an XPath expression. */
@:native("System.Xml.XPath.XPathException")
extern class XPathException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
	/**
	 * Streams all the  properties into the  class for the specified .
	 * @param info The  object.
	 * @param context The  object.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
