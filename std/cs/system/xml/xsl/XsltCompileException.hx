package cs.system.xml.xsl;

/** The exception that is thrown by the Load method when an error is found in the XSLT style sheet. */
@:native("System.Xml.Xsl.XsltCompileException")
extern class XsltCompileException extends cs.system.xml.xsl.XsltException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(inner:cs.system.Exception, sourceUri:String, lineNumber:Int, linePosition:Int):Void;
	/**
	 * Streams all the  properties into the  class for the given .
	 * @param info The  object.
	 * @param context The  object.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
