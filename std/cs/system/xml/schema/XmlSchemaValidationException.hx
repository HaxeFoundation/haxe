package cs.system.xml.schema;

/** Represents the exception thrown when XML Schema Definition Language (XSD) schema validation errors and warnings are encountered in an XML document being validated. */
@:native("System.Xml.Schema.XmlSchemaValidationException")
extern class XmlSchemaValidationException extends cs.system.xml.schema.XmlSchemaException {
	/**
	 * Gets the XML node that caused this .
	 * @return The XML node that caused this .
	 */
	var SourceObject(default, never):Dynamic;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, innerException:cs.system.Exception, lineNumber:Int, linePosition:Int):Void;
	/**
	 * Constructs a new  object with the given  and  information that contains all the
	 * properties of the .
	 * @param info 
	 * @param context 
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
