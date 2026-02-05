package cs.system.net.networkinformation;

/** Provides Internet Control Message Protocol for Internet Protocol version 6 (ICMPv6) statistical data for the local computer. */
@:native("System.Net.NetworkInformation.IcmpV6Statistics")
extern class IcmpV6Statistics {
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) messages
	 * received because of a packet having an unreachable address in its destination.
	 * @return An  value that specifies the total number of Destination Unreachable
	 * messages received.
	 */
	var DestinationUnreachableMessagesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) messages
	 * sent because of a packet having an unreachable address in its destination.
	 * @return An  value that specifies the total number of Destination Unreachable
	 * messages sent.
	 */
	var DestinationUnreachableMessagesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Echo
	 * Reply messages received.
	 * @return An  value that specifies the total number of number of ICMP Echo Reply
	 * messages received.
	 */
	var EchoRepliesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Echo
	 * Reply messages sent.
	 * @return An  value that specifies the total number of number of ICMP Echo Reply
	 * messages sent.
	 */
	var EchoRepliesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Echo
	 * Request messages received.
	 * @return An  value that specifies the total number of number of ICMP Echo Request
	 * messages received.
	 */
	var EchoRequestsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Echo
	 * Request messages sent.
	 * @return An  value that specifies the total number of number of ICMP Echo Request
	 * messages sent.
	 */
	var EchoRequestsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) error
	 * messages received.
	 * @return An  value that specifies the total number of ICMP error messages
	 * received.
	 */
	var ErrorsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) error
	 * messages sent.
	 * @return An  value that specifies the total number of ICMP error messages sent.
	 */
	var ErrorsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Group management Protocol (IGMP) Group Membership
	 * Query messages received.
	 * @return An  value that specifies the total number of Group Membership Query
	 * messages received.
	 */
	var MembershipQueriesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Group management Protocol (IGMP) Group Membership
	 * Query messages sent.
	 * @return An  value that specifies the total number of Group Membership Query
	 * messages sent.
	 */
	var MembershipQueriesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Group Management Protocol (IGMP) Group Membership
	 * Reduction messages received.
	 * @return An  value that specifies the total number of Group Membership Reduction
	 * messages received.
	 */
	var MembershipReductionsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Group Management Protocol (IGMP) Group Membership
	 * Reduction messages sent.
	 * @return An  value that specifies the total number of Group Membership Reduction
	 * messages sent.
	 */
	var MembershipReductionsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Group Management Protocol (IGMP) Group Membership
	 * Report messages received.
	 * @return An  value that specifies the total number of Group Membership Report
	 * messages received.
	 */
	var MembershipReportsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Group Management Protocol (IGMP) Group Membership
	 * Report messages sent.
	 * @return An  value that specifies the total number of Group Membership Report
	 * messages sent.
	 */
	var MembershipReportsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) messages
	 * received.
	 * @return An  value that specifies the total number of ICMPv6 messages received.
	 */
	var MessagesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) messages
	 * sent.
	 * @return An  value that specifies the total number of ICMPv6 messages sent.
	 */
	var MessagesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Neighbor
	 * Advertisement messages received.
	 * @return An  value that specifies the total number of ICMP Neighbor Advertisement
	 * messages received.
	 */
	var NeighborAdvertisementsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Neighbor
	 * Advertisement messages sent.
	 * @return An  value that specifies the total number of Neighbor Advertisement
	 * messages sent.
	 */
	var NeighborAdvertisementsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Neighbor
	 * Solicitation messages received.
	 * @return An  value that specifies the total number of Neighbor Solicitation
	 * messages received.
	 */
	var NeighborSolicitsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Neighbor
	 * Solicitation messages sent.
	 * @return An  value that specifies the total number of Neighbor Solicitation
	 * messages sent.
	 */
	var NeighborSolicitsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Packet
	 * Too Big messages received.
	 * @return An  value that specifies the total number of ICMP Packet Too Big
	 * messages received.
	 */
	var PacketTooBigMessagesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Packet
	 * Too Big messages sent.
	 * @return An  value that specifies the total number of ICMP Packet Too Big
	 * messages sent.
	 */
	var PacketTooBigMessagesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6)
	 * Parameter Problem messages received.
	 * @return An  value that specifies the total number of ICMP Parameter Problem
	 * messages received.
	 */
	var ParameterProblemsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6)
	 * Parameter Problem messages sent.
	 * @return An  value that specifies the total number of ICMP Parameter Problem
	 * messages sent.
	 */
	var ParameterProblemsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Redirect
	 * messages received.
	 * @return An  value that specifies the total number of ICMP Redirect messages
	 * received.
	 */
	var RedirectsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Redirect
	 * messages sent.
	 * @return An  value that specifies the total number of ICMP Redirect messages
	 * sent.
	 */
	var RedirectsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Router
	 * Advertisement messages received.
	 * @return An  value that specifies the total number of Router Advertisement
	 * messages received.
	 */
	var RouterAdvertisementsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Router
	 * Advertisement messages sent.
	 * @return An  value that specifies the total number of Router Advertisement
	 * messages sent.
	 */
	var RouterAdvertisementsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Router
	 * Solicitation messages received.
	 * @return An  value that specifies the total number of Router Solicitation
	 * messages received.
	 */
	var RouterSolicitsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Router
	 * Solicitation messages sent.
	 * @return An  value that specifies the total number of Router Solicitation
	 * messages sent.
	 */
	var RouterSolicitsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Time
	 * Exceeded messages received.
	 * @return An  value that specifies the total number of ICMP Time Exceeded messages
	 * received.
	 */
	var TimeExceededMessagesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 6 (ICMPv6) Time
	 * Exceeded messages sent.
	 * @return An  value that specifies the total number of ICMP Time Exceeded messages
	 * sent.
	 */
	var TimeExceededMessagesSent(default, never):haxe.Int64;
}
