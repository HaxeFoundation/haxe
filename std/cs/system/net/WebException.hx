package cs.system.net;

/** The exception that is thrown when an error occurs while accessing the network through a pluggable protocol. */
@:native("System.Net.WebException")
extern class WebException extends cs.system.InvalidOperationException {
	/**
	 * Gets the response that the remote host returned.
	 * @return If a response is available from the Internet resource, a  instance that
	 * contains the error response from an Internet resource; otherwise, .
	 */
	var Response(default, never):cs.system.net.WebResponse;
	/**
	 * Gets the status of the response.
	 * @return One of the  values.
	 */
	var Status(default, never):cs.system.net.WebExceptionStatus;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(message:String, status:cs.system.net.WebExceptionStatus):Void {})
	function new(message:String, innerException:cs.system.Exception, status:cs.system.net.WebExceptionStatus, response:cs.system.net.WebResponse):Void;
	/**
	 * Populates a  instance with the data needed to serialize the .
	 * @param serializationInfo The  to be used.
	 * @param streamingContext The  to be used.
	 */
	function GetObjectData(serializationInfo:cs.system.runtime.serialization.SerializationInfo, streamingContext:cs.system.runtime.serialization.StreamingContext):Void;
}
