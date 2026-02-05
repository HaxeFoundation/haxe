package cs.system.net.sockets;

/** Represents an element in a  array. */
@:native("System.Net.Sockets.SendPacketsElement")
extern class SendPacketsElement {
	/**
	 * Gets the buffer to be sent if the  object was initialized with a  parameter.
	 * @return The byte buffer to send if the  object was initialized with a 
	 * parameter.
	 */
	var Buffer(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the count of bytes to be sent.
	 * @return The count of bytes to send if the  class was initialized with a 
	 * parameter.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a Boolean value that indicates if this element should not be combined with
	 * the next element in a single send request from the sockets layer to the
	 * transport.
	 * @return A Boolean value that indicates if this element should not be combined
	 * with the next element in a single send request.
	 */
	var EndOfPacket(default, never):Bool;
	/**
	 * Gets the filename of the file to send if the  object was initialized with a 
	 * parameter.
	 * @return The filename of the file to send if the  object was initialized with a 
	 * parameter.
	 */
	var FilePath(default, never):String;
	/**
	 * Gets the offset, in bytes, from the beginning of the data buffer or file to the
	 * location in the buffer or file to start sending the data.
	 * @return The offset, in bytes, from the beginning of the data buffer or file to
	 * the location in the buffer or file to start sending the data.
	 */
	var Offset(default, never):Int;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(filepath:String):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void {})
	@:overload(function(filepath:String, offset:Int, count:Int):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, endOfPacket:Bool):Void {})
	function new(filepath:String, offset:Int, count:Int, endOfPacket:Bool):Void;
}
