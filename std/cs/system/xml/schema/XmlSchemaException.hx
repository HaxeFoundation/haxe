package cs.system.xml.schema;

/** Returns detailed information about the schema exception. */
@:native("System.Xml.Schema.XmlSchemaException")
extern class XmlSchemaException extends cs.system.SystemException {
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
	 * The  that produced the .
	 * @return A valid object instance represents a structural validation error in the
	 * XML Schema Object Model (SOM).
	 */
	var SourceSchemaObject(default, never):cs.system.xml.schema.XmlSchemaObject;
	/**
	 * Gets the Uniform Resource Identifier (URI) location of the schema that caused
	 * the exception.
	 * @return The URI location of the schema that caused the exception.
	 */
	var SourceUri(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, innerException:cs.system.Exception, lineNumber:Int, linePosition:Int):Void;
	/**
	 * Streams all the  properties into the  class for the given .
	 * @param info The .
	 * @param context The  information.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
