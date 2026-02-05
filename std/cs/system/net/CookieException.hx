package cs.system.net;

/** The exception that is thrown when an error is made adding a  to a . */
@:native("System.Net.CookieException")
extern class CookieException extends cs.system.FormatException {
	function new():Void;
	/**
	 * Populates a  instance with the data needed to serialize the .
	 * @param serializationInfo The object that holds the serialized object data. The 
	 * to populate with data.
	 * @param streamingContext The contextual information about the source or
	 * destination. A  that specifies the destination for this serialization.
	 */
	function GetObjectData(serializationInfo:cs.system.runtime.serialization.SerializationInfo, streamingContext:cs.system.runtime.serialization.StreamingContext):Void;
}
