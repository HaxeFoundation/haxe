package cs.system.net;

/** Stores serialized information from  derived classes. */
@:native("System.Net.SocketAddress")
extern class SocketAddress {
	/**
	 * Gets the  enumerated value of the current .
	 * @return One of the  enumerated values.
	 */
	var Family(default, never):cs.system.net.sockets.AddressFamily;
	/**
	 * Gets the underlying buffer size of the .
	 * @return The underlying buffer size of the .
	 */
	var Size(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):cs.UInt8;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.UInt8):Void;
	@:overload(function(family:cs.system.net.sockets.AddressFamily):Void {})
	function new(family:cs.system.net.sockets.AddressFamily, size:Int):Void;
	/**
	 * Determines whether the specified  is equal to the current .
	 * @param comparand The  to compare with the current .
	 * @return if the specified  is equal to the current ; otherwise, .
	 */
	function Equals(comparand:Dynamic):Bool;
	/**
	 * Serves as a hash function for a particular type, suitable for use in hashing
	 * algorithms and data structures like a hash table.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns information about the socket address.
	 * @return A string that contains information about the .
	 */
	function ToString():String;
}
