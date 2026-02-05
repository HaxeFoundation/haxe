package cs.system.net.networkinformation;

/** Provides Internet Protocol (IP) statistical data. */
@:native("System.Net.NetworkInformation.IPGlobalStatistics")
extern class IPGlobalStatistics {
	/**
	 * Gets the default time-to-live (TTL) value for Internet Protocol (IP) packets.
	 * @return An  value that specifies the TTL.
	 */
	var DefaultTtl(default, never):Int;
	/**
	 * Gets a  value that specifies whether Internet Protocol (IP) packet forwarding is
	 * enabled.
	 * @return A  value that specifies whether packet forwarding is enabled.
	 */
	var ForwardingEnabled(default, never):Bool;
	/**
	 * Gets the number of network interfaces.
	 * @return An  value containing the number of network interfaces for the address
	 * family used to obtain this  instance.
	 */
	var NumberOfInterfaces(default, never):Int;
	/**
	 * Gets the number of Internet Protocol (IP) addresses assigned to the local
	 * computer.
	 * @return An  value that indicates the number of IP addresses assigned to the
	 * address family (Internet Protocol version 4 or Internet Protocol version 6)
	 * described by this object.
	 */
	var NumberOfIPAddresses(default, never):Int;
	/**
	 * Gets the number of routes in the Internet Protocol (IP) routing table.
	 * @return An  value that specifies the total number of routes in the routing
	 * table.
	 */
	var NumberOfRoutes(default, never):Int;
	/**
	 * Gets the number of outbound Internet Protocol (IP) packets.
	 * @return An  value that specifies the total number of outgoing packets.
	 */
	var OutputPacketRequests(default, never):haxe.Int64;
	/**
	 * Gets the number of routes that have been discarded from the routing table.
	 * @return An  value that specifies the total number of valid routes that have been
	 * discarded.
	 */
	var OutputPacketRoutingDiscards(default, never):haxe.Int64;
	/**
	 * Gets the number of transmitted Internet Protocol (IP) packets that have been
	 * discarded.
	 * @return An  value that specifies the total number of outgoing packets that have
	 * been discarded.
	 */
	var OutputPacketsDiscarded(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets for which the local computer
	 * could not determine a route to the destination address.
	 * @return An  value that specifies the number of packets that could not be sent
	 * because a route could not be found.
	 */
	var OutputPacketsWithNoRoute(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets that could not be fragmented.
	 * @return An  value that specifies the total number of packets that required
	 * fragmentation but had the "Don't Fragment" bit set.
	 */
	var PacketFragmentFailures(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets that required reassembly.
	 * @return An  value that specifies the total number of packet reassemblies
	 * required.
	 */
	var PacketReassembliesRequired(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets that were not successfully
	 * reassembled.
	 * @return An  value that specifies the total number of packets that could not be
	 * reassembled.
	 */
	var PacketReassemblyFailures(default, never):haxe.Int64;
	/**
	 * Gets the maximum amount of time within which all fragments of an Internet
	 * Protocol (IP) packet must arrive.
	 * @return An  value that specifies the maximum number of milliseconds within which
	 * all fragments of a packet must arrive to avoid being discarded.
	 */
	var PacketReassemblyTimeout(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets fragmented.
	 * @return An  value that specifies the total number of fragmented packets.
	 */
	var PacketsFragmented(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets reassembled.
	 * @return An  value that specifies the total number of fragmented packets that
	 * have been successfully reassembled.
	 */
	var PacketsReassembled(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets received.
	 * @return An  value that specifies the total number of IP packets received.
	 */
	var ReceivedPackets(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets delivered.
	 * @return An  value that specifies the total number of IP packets delivered.
	 */
	var ReceivedPacketsDelivered(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets that have been received and
	 * discarded.
	 * @return An  value that specifies the total number of incoming packets that have
	 * been discarded.
	 */
	var ReceivedPacketsDiscarded(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets forwarded.
	 * @return An  value that specifies the total number of forwarded packets.
	 */
	var ReceivedPacketsForwarded(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets with address errors that were
	 * received.
	 * @return An  value that specifies the total number of IP packets received with
	 * errors in the address portion of the header.
	 */
	var ReceivedPacketsWithAddressErrors(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets with header errors that were
	 * received.
	 * @return An  value that specifies the total number of IP packets received and
	 * discarded due to errors in the header.
	 */
	var ReceivedPacketsWithHeadersErrors(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Protocol (IP) packets received on the local machine
	 * with an unknown protocol in the header.
	 * @return An  value that indicates the total number of IP packets received with an
	 * unknown protocol.
	 */
	var ReceivedPacketsWithUnknownProtocol(default, never):haxe.Int64;
}
