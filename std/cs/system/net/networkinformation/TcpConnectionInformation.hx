package cs.system.net.networkinformation;

/** Provides information about the Transmission Control Protocol (TCP) connections on the local computer. */
@:native("System.Net.NetworkInformation.TcpConnectionInformation")
extern class TcpConnectionInformation {
	/**
	 * Gets the local endpoint of a Transmission Control Protocol (TCP) connection.
	 * @return An  instance that contains the IP address and port on the local
	 * computer.
	 */
	var LocalEndPoint(default, never):cs.system.net.IPEndPoint;
	/**
	 * Gets the remote endpoint of a Transmission Control Protocol (TCP) connection.
	 * @return An  instance that contains the IP address and port on the remote
	 * computer.
	 */
	var RemoteEndPoint(default, never):cs.system.net.IPEndPoint;
	/**
	 * Gets the state of this Transmission Control Protocol (TCP) connection.
	 * @return One of the  enumeration values.
	 */
	var State(default, never):cs.system.net.networkinformation.TcpState;
}
