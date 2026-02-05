package cs.system.net;

/** Identifies a network address. This is an  class. */
@:native("System.Net.EndPoint")
extern class EndPoint {
	/**
	 * Gets the address family to which the endpoint belongs.
	 * @return One of the  values.
	 */
	var AddressFamily(default, never):cs.system.net.sockets.AddressFamily;
	/**
	 * Creates an  instance from a  instance.
	 * @param socketAddress The socket address that serves as the endpoint for a
	 * connection.
	 * @return A new  instance that is initialized from the specified  instance.
	 */
	function Create(socketAddress:cs.system.net.SocketAddress):cs.system.net.EndPoint;
	/**
	 * Serializes endpoint information into a  instance.
	 * @return A  instance that contains the endpoint information.
	 */
	function Serialize():cs.system.net.SocketAddress;
}
