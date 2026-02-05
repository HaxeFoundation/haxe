package cs.system.net.networkinformation;

/** Provides statistical data for a network interface on the local computer. */
@:native("System.Net.NetworkInformation.IPv4InterfaceStatistics")
extern class IPv4InterfaceStatistics {
	/**
	 * Gets the number of bytes that were received on the interface.
	 * @return An  value that specifies the total number of bytes that were received on
	 * the interface.
	 */
	var BytesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of bytes that were sent on the interface.
	 * @return An  value that specifies the total number of bytes that were transmitted
	 * on the interface.
	 */
	var BytesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of incoming packets that were discarded.
	 * @return An  value that specifies the total number of discarded incoming packets.
	 */
	var IncomingPacketsDiscarded(default, never):haxe.Int64;
	/**
	 * Gets the number of incoming packets with errors.
	 * @return An  value that specifies the total number of incoming packets with
	 * errors.
	 */
	var IncomingPacketsWithErrors(default, never):haxe.Int64;
	/**
	 * Gets the number of incoming packets with an unknown protocol that were received
	 * on the interface.
	 * @return An  value that specifies the total number of incoming packets with an
	 * unknown protocol.
	 */
	var IncomingUnknownProtocolPackets(default, never):haxe.Int64;
	/**
	 * Gets the number of non-unicast packets that were received on the interface.
	 * @return An  value that specifies the total number of non-unicast packets that
	 * were received on the interface.
	 */
	var NonUnicastPacketsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of non-unicast packets that were sent on the interface.
	 * @return An  value that specifies the total number of non-unicast packets that
	 * were sent on the interface.
	 */
	var NonUnicastPacketsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of outgoing packets that were discarded.
	 * @return An  value that specifies the total number of discarded outgoing packets.
	 */
	var OutgoingPacketsDiscarded(default, never):haxe.Int64;
	/**
	 * Gets the number of outgoing packets with errors.
	 * @return An  value that specifies the total number of outgoing packets with
	 * errors.
	 */
	var OutgoingPacketsWithErrors(default, never):haxe.Int64;
	/**
	 * Gets the length of the output queue.
	 * @return An  value that specifies the total number of packets in the output
	 * queue.
	 */
	var OutputQueueLength(default, never):haxe.Int64;
	/**
	 * Gets the number of unicast packets that were received on the interface.
	 * @return An  value that specifies the total number of unicast packets that were
	 * received on the interface.
	 */
	var UnicastPacketsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of unicast packets that were sent on the interface.
	 * @return An  value that specifies the total number of unicast packets that were
	 * sent on the interface.
	 */
	var UnicastPacketsSent(default, never):haxe.Int64;
}
