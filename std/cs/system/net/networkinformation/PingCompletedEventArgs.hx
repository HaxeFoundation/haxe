package cs.system.net.networkinformation;

/** Provides data for the  event. */
@:native("System.Net.NetworkInformation.PingCompletedEventArgs")
extern class PingCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets an object that contains data that describes an attempt to send an Internet
	 * Control Message Protocol (ICMP) echo request message and receive a corresponding
	 * ICMP echo reply message.
	 * @return A  object that describes the results of the ICMP echo request.
	 */
	var Reply(default, never):cs.system.net.networkinformation.PingReply;
}
