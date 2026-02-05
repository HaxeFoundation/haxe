package cs.system.net;

/** The exception that is thrown when an error is made while using a network protocol. */
@:native("System.Net.ProtocolViolationException")
extern class ProtocolViolationException extends cs.system.InvalidOperationException {
	@:overload(function():Void {})
	function new(message:String):Void;
	/**
	 * Populates a  with the data required to serialize the target object.
	 * @param serializationInfo The  to populate with data.
	 * @param streamingContext A  that specifies the destination for this
	 * serialization.
	 */
	function GetObjectData(serializationInfo:cs.system.runtime.serialization.SerializationInfo, streamingContext:cs.system.runtime.serialization.StreamingContext):Void;
}
