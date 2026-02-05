package cs.system.net;

/** Represents a network endpoint as an IP address and a port number. */
@:native("System.Net.IPEndPoint")
extern class IPEndPoint extends cs.system.net.EndPoint {
	/** Specifies the maximum value that can be assigned to the  property. The MaxPort value is set to 0x0000FFFF. This field is read-only. */
	static var MaxPort(default, never):Int;
	/** Specifies the minimum value that can be assigned to the  property. This field is read-only. */
	static var MinPort(default, never):Int;
	/**
	 * Gets or sets the IP address of the endpoint.
	 * @return An  instance containing the IP address of the endpoint.
	 */
	var Address(default, default):cs.system.net.IPAddress;
	/**
	 * Gets or sets the port number of the endpoint.
	 * @return An integer value in the range  to  indicating the port number of the
	 * endpoint.
	 */
	var Port(default, default):Int;
	@:overload(function(address:haxe.Int64, port:Int):Void {})
	function new(address:cs.system.net.IPAddress, port:Int):Void;
	/**
	 * Creates an endpoint from a socket address.
	 * @param socketAddress The  to use for the endpoint.
	 * @return An  instance using the specified socket address.
	 */
	function Create(socketAddress:cs.system.net.SocketAddress):cs.system.net.EndPoint;
	/**
	 * Determines whether the specified  is equal to the current .
	 * @param comparand The  to compare with the current .
	 * @return if the specified  is equal to the current ; otherwise, .
	 */
	function Equals(comparand:Dynamic):Bool;
	/**
	 * Returns a hash value for a  instance.
	 * @return An integer hash value.
	 */
	function GetHashCode():Int;
	/**
	 * Serializes endpoint information into a  instance.
	 * @return A  instance containing the socket address for the endpoint.
	 */
	function Serialize():cs.system.net.SocketAddress;
	/**
	 * Returns the IP address and port number of the specified endpoint.
	 * @return A string containing the IP address and the port number of the specified
	 * endpoint (for example, 192.168.1.2:80).
	 */
	function ToString():String;
}
