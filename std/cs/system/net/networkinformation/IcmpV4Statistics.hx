package cs.system.net.networkinformation;

/** Provides Internet Control Message Protocol for IPv4 (ICMPv4) statistical data for the local computer. */
@:native("System.Net.NetworkInformation.IcmpV4Statistics")
extern class IcmpV4Statistics {
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Address
	 * Mask Reply messages that were received.
	 * @return An  value that specifies the total number of Address Mask Reply messages
	 * that were received.
	 */
	var AddressMaskRepliesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Address
	 * Mask Reply messages that were sent.
	 * @return An  value that specifies the total number of Address Mask Reply messages
	 * that were sent.
	 */
	var AddressMaskRepliesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Address
	 * Mask Request messages that were received.
	 * @return An  value that specifies the total number of Address Mask Request
	 * messages that were received.
	 */
	var AddressMaskRequestsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Address
	 * Mask Request messages that were sent.
	 * @return An  value that specifies the total number of Address Mask Request
	 * messages that were sent.
	 */
	var AddressMaskRequestsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) messages
	 * that were received because of a packet having an unreachable address in its
	 * destination.
	 * @return An  value that specifies the total number of Destination Unreachable
	 * messages that were received.
	 */
	var DestinationUnreachableMessagesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) messages
	 * that were sent because of a packet having an unreachable address in its
	 * destination.
	 * @return An  value that specifies the total number of Destination Unreachable
	 * messages sent.
	 */
	var DestinationUnreachableMessagesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Echo
	 * Reply messages that were received.
	 * @return An  value that specifies the total number of number of ICMP Echo Reply
	 * messages that were received.
	 */
	var EchoRepliesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Echo
	 * Reply messages that were sent.
	 * @return An  value that specifies the total number of number of ICMP Echo Reply
	 * messages that were sent.
	 */
	var EchoRepliesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Echo
	 * Request messages that were received.
	 * @return An  value that specifies the total number of number of ICMP Echo Request
	 * messages that were received.
	 */
	var EchoRequestsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Echo
	 * Request messages that were sent.
	 * @return An  value that specifies the total number of number of ICMP Echo Request
	 * messages that were sent.
	 */
	var EchoRequestsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) error
	 * messages that were received.
	 * @return An  value that specifies the total number of ICMP error messages that
	 * were received.
	 */
	var ErrorsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) error
	 * messages that were sent.
	 * @return An  value that specifies the total number of number of ICMP error
	 * messages that were sent.
	 */
	var ErrorsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol messages that were
	 * received.
	 * @return An  value that specifies the total number of ICMPv4 messages that were
	 * received.
	 */
	var MessagesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) messages
	 * that were sent.
	 * @return An  value that specifies the total number of ICMPv4 messages that were
	 * sent.
	 */
	var MessagesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4)
	 * Parameter Problem messages that were received.
	 * @return An  value that specifies the total number of ICMP Parameter Problem
	 * messages that were received.
	 */
	var ParameterProblemsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4)
	 * Parameter Problem messages that were sent.
	 * @return An  value that specifies the total number of ICMP Parameter Problem
	 * messages that were sent.
	 */
	var ParameterProblemsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Redirect
	 * messages that were received.
	 * @return An  value that specifies the total number of ICMP Redirect messages that
	 * were received.
	 */
	var RedirectsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Redirect
	 * messages that were sent.
	 * @return An  value that specifies the total number of ICMP Redirect messages that
	 * were sent.
	 */
	var RedirectsSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Source
	 * Quench messages that were received.
	 * @return An  value that specifies the total number of Source Quench messages that
	 * were received.
	 */
	var SourceQuenchesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Source
	 * Quench messages that were sent.
	 * @return An  value that specifies the total number of Source Quench messages that
	 * were sent.
	 */
	var SourceQuenchesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Time
	 * Exceeded messages that were received.
	 * @return An  value that specifies the total number of ICMP Time Exceeded messages
	 * that were received.
	 */
	var TimeExceededMessagesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4) Time
	 * Exceeded messages that were sent.
	 * @return An  value that specifies the total number of ICMP Time Exceeded messages
	 * that were sent.
	 */
	var TimeExceededMessagesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4)
	 * Timestamp Reply messages that were received.
	 * @return An  value that specifies the total number of Timestamp Reply messages
	 * that were received.
	 */
	var TimestampRepliesReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4)
	 * Timestamp Reply messages that were sent.
	 * @return An  value that specifies the total number of Timestamp Reply messages
	 * that were sent.
	 */
	var TimestampRepliesSent(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4)
	 * Timestamp Request messages that were received.
	 * @return An  value that specifies the total number of Timestamp Request messages
	 * that were received.
	 */
	var TimestampRequestsReceived(default, never):haxe.Int64;
	/**
	 * Gets the number of Internet Control Message Protocol version 4 (ICMPv4)
	 * Timestamp Request messages that were sent.
	 * @return An  value that specifies the total number of Timestamp Request messages
	 * that were sent.
	 */
	var TimestampRequestsSent(default, never):haxe.Int64;
}
