package cs.system.net;

/** Represents a network endpoint as a host name or a string representation of an IP address and a port number. */
@:native("System.Net.DnsEndPoint")
extern class DnsEndPoint extends cs.system.net.EndPoint {
	/**
	 * Gets the host name or string representation of the Internet Protocol (IP)
	 * address of the host.
	 * @return A host name or string representation of an IP address.
	 */
	var Host(default, never):String;
	/**
	 * Gets the port number of the .
	 * @return An integer value in the range 0 to 0xffff indicating the port number of
	 * the .
	 */
	var Port(default, never):Int;
	@:overload(function(host:String, port:Int):Void {})
	function new(host:String, port:Int, addressFamily:cs.system.net.sockets.AddressFamily):Void;
	/**
	 * Compares two  objects.
	 * @param comparand A  instance to compare to the current instance.
	 * @return if the two  instances are equal; otherwise, .
	 */
	function Equals(comparand:Dynamic):Bool;
	/**
	 * Returns a hash value for a .
	 * @return An integer hash value for the .
	 */
	function GetHashCode():Int;
	/**
	 * Returns the host name or string representation of the IP address and port number
	 * of the .
	 * @return A string containing the address family, host name or IP address string,
	 * and the port number of the specified .
	 */
	function ToString():String;
}
