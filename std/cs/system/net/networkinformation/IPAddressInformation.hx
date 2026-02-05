package cs.system.net.networkinformation;

/** Provides information about a network interface address. */
@:native("System.Net.NetworkInformation.IPAddressInformation")
extern class IPAddressInformation {
	/**
	 * Gets the Internet Protocol (IP) address.
	 * @return An  instance that contains the IP address of an interface.
	 */
	var Address(default, never):cs.system.net.IPAddress;
	/**
	 * Gets a  value that indicates whether the Internet Protocol (IP) address is valid
	 * to appear in a Domain Name System (DNS) server database.
	 * @return if the address can appear in a DNS database; otherwise, .
	 */
	var IsDnsEligible(default, never):Bool;
	/**
	 * Gets a  value that indicates whether the Internet Protocol (IP) address is
	 * transient (a cluster address).
	 * @return if the address is transient; otherwise, .
	 */
	var IsTransient(default, never):Bool;
}
