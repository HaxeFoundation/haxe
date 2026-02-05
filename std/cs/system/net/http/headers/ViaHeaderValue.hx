package cs.system.net.http.headers;

/** Represents the value of a Via header. */
@:native("System.Net.Http.Headers.ViaHeaderValue")
extern class ViaHeaderValue {
	/**
	 * Gets the comment field used to identify the software of the recipient proxy or
	 * gateway.
	 * @return The comment field used to identify the software of the recipient proxy
	 * or gateway.
	 */
	var Comment(default, never):String;
	/**
	 * Gets the protocol name of the received protocol.
	 * @return The protocol name.
	 */
	var ProtocolName(default, never):String;
	/**
	 * Gets the protocol version of the received protocol.
	 * @return The protocol version.
	 */
	var ProtocolVersion(default, never):String;
	/**
	 * Gets the host and port that the request or response was received by.
	 * @return The host and port that the request or response was received by.
	 */
	var ReceivedBy(default, never):String;
	@:overload(function(protocolVersion:String, receivedBy:String):Void {})
	@:overload(function(protocolVersion:String, receivedBy:String, protocolName:String):Void {})
	function new(protocolVersion:String, receivedBy:String, protocolName:String, comment:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents via header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.ViaHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.ViaHeaderValue>):Bool;
	/**
	 * Determines whether the specified  is equal to the current  object.
	 * @param obj The object to compare with the current object.
	 * @return if the specified  is equal to the current object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for an  object.
	 * @return A hash code for the current object.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current object.
	 */
	function ToString():String;
}
