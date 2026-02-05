package cs.system.net.networkinformation;

/** Provides Internet Protocol (IP) statistical data for an network interface on the local computer. */
@:native("System.Net.NetworkInformation.IPInterfaceStatistics")
extern class IPInterfaceStatistics {
	/**
	 * Gets the number of bytes that were received on the interface.
	 * @return The total number of bytes that were received on the interface.
	 */
	var BytesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of bytes that were sent on the interface.
	 * @return The total number of bytes that were sent on the interface.
	 */
	var BytesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of incoming packets that were discarded.
	 * @return The total number of incoming packets that were discarded.
	 */
	var IncomingPacketsDiscarded(default, never):haxe.Int64;
	/**
	 * Gets the number of incoming packets with errors.
	 * @return The total number of incoming packets with errors.
	 */
	var IncomingPacketsWithErrors(default, never):haxe.Int64;
	/**
	 * Gets the number of incoming packets with an unknown protocol that were received
	 * on the interface.
	 * @return The total number of incoming packets with an unknown protocol that were
	 * received on the interface.
	 */
	var IncomingUnknownProtocolPackets(default, never):haxe.Int64;
	/**
	 * Gets the number of non-unicast packets that were received on the interface.
	 * @return The total number of incoming non-unicast packets received on the
	 * interface.
	 */
	var NonUnicastPacketsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of non-unicast packets that were sent on the interface.
	 * @return The total number of non-unicast packets that were sent on the interface.
	 */
	var NonUnicastPacketsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of outgoing packets that were discarded.
	 * @return The total number of outgoing packets that were discarded.
	 */
	var OutgoingPacketsDiscarded(default, never):haxe.Int64;
	/**
	 * Gets the number of outgoing packets with errors.
	 * @return The total number of outgoing packets with errors.
	 */
	var OutgoingPacketsWithErrors(default, never):haxe.Int64;
	/**
	 * Gets the length of the output queue.
	 * @return The total number of packets in the output queue.
	 */
	var OutputQueueLength(default, never):haxe.Int64;
	/**
	 * Gets the number of unicast packets that were received on the interface.
	 * @return The total number of unicast packets that were received on the interface.
	 */
	var UnicastPacketsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of unicast packets that were sent on the interface.
	 * @return The total number of unicast packets that were sent on the interface.
	 */
	var UnicastPacketsSent(default, never):haxe.Int64;
}
