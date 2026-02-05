package cs.system.net.networkinformation;

/** Provides User Datagram Protocol (UDP) statistical data. */
@:native("System.Net.NetworkInformation.UdpStatistics")
extern class UdpStatistics {
	/**
	 * Gets the number of User Datagram Protocol (UDP) datagrams that were received.
	 * @return An  value that specifies the total number of datagrams that were
	 * delivered to UDP users.
	 */
	var DatagramsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of User Datagram Protocol (UDP) datagrams that were sent.
	 * @return An  value that specifies the total number of datagrams that were sent.
	 */
	var DatagramsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of User Datagram Protocol (UDP) datagrams that were received and
	 * discarded because of port errors.
	 * @return An  value that specifies the total number of received UDP datagrams that
	 * were discarded because there was no listening application at the destination
	 * port.
	 */
	var IncomingDatagramsDiscarded(default, never):haxe.Int64;
	/**
	 * Gets the number of User Datagram Protocol (UDP) datagrams that were received and
	 * discarded because of errors other than bad port information.
	 * @return An  value that specifies the total number of received UDP datagrams that
	 * could not be delivered for reasons other than the lack of an application at the
	 * destination port.
	 */
	var IncomingDatagramsWithErrors(default, never):haxe.Int64;
	/**
	 * Gets the number of local endpoints that are listening for User Datagram Protocol
	 * (UDP) datagrams.
	 * @return An  value that specifies the total number of sockets that are listening
	 * for UDP datagrams.
	 */
	var UdpListeners(default, never):Int;
}
