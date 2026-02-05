package cs.system.net.networkinformation;

/** Provides information about the status and data resulting from a  or  operation. */
@:native("System.Net.NetworkInformation.PingReply")
extern class PingReply {
	/**
	 * Gets the address of the host that sends the Internet Control Message Protocol
	 * (ICMP) echo reply.
	 * @return An  containing the destination for the ICMP echo message.
	 */
	var Address(default, never):cs.system.net.IPAddress;
	/**
	 * Gets the buffer of data received in an Internet Control Message Protocol (ICMP)
	 * echo reply message.
	 * @return A  array containing the data received in an ICMP echo reply message, or
	 * an empty array, if no reply was received.
	 */
	var Buffer(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the options used to transmit the reply to an Internet Control Message
	 * Protocol (ICMP) echo request.
	 * @return A  object that contains the Time to Live (TTL) and the fragmentation
	 * directive used for transmitting the reply if  is ; otherwise, .
	 */
	var Options(default, never):cs.system.net.networkinformation.PingOptions;
	/**
	 * Gets the number of milliseconds taken to send an Internet Control Message
	 * Protocol (ICMP) echo request and receive the corresponding ICMP echo reply
	 * message.
	 * @return An  that specifies the round trip time, in milliseconds.
	 */
	var RoundtripTime(default, never):haxe.Int64;
	/**
	 * Gets the status of an attempt to send an Internet Control Message Protocol
	 * (ICMP) echo request and receive the corresponding ICMP echo reply message.
	 * @return An  value indicating the result of the request.
	 */
	var Status(default, never):cs.system.net.networkinformation.IPStatus;
}
