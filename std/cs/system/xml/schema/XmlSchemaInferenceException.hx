package cs.system.xml.schema;

/** Returns information about errors encountered by the  class while inferring a schema from an XML document. */
@:native("System.Xml.Schema.XmlSchemaInferenceException")
extern class XmlSchemaInferenceException extends cs.system.xml.schema.XmlSchemaException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, innerException:cs.system.Exception, lineNumber:Int, linePosition:Int):Void;
	/**
	 * Streams all the  object properties into the  object specified for the  object
	 * specified.
	 * @param info A  object.
	 * @param context A  object.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
