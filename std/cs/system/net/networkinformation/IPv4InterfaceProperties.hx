package cs.system.net.networkinformation;

/** Provides information about network interfaces that support Internet Protocol version 4 (IPv4). */
@:native("System.Net.NetworkInformation.IPv4InterfaceProperties")
extern class IPv4InterfaceProperties {
	/**
	 * Gets the index of the network interface associated with the Internet Protocol
	 * version 4 (IPv4) address.
	 * @return An  that contains the index of the IPv4 interface.
	 */
	var Index(default, never):Int;
	/**
	 * Gets a  value that indicates whether this interface has an automatic private IP
	 * addressing (APIPA) address.
	 * @return if the interface uses an APIPA address; otherwise, .
	 */
	var IsAutomaticPrivateAddressingActive(default, never):Bool;
	/**
	 * Gets a  value that indicates whether this interface has automatic private IP
	 * addressing (APIPA) enabled.
	 * @return if the interface uses APIPA; otherwise, .
	 */
	var IsAutomaticPrivateAddressingEnabled(default, never):Bool;
	/**
	 * Gets a  value that indicates whether the interface is configured to use a
	 * Dynamic Host Configuration Protocol (DHCP) server to obtain an IP address.
	 * @return if the interface is configured to obtain an IP address from a DHCP
	 * server; otherwise, .
	 */
	var IsDhcpEnabled(default, never):Bool;
	/**
	 * Gets a  value that indicates whether this interface can forward (route) packets.
	 * @return if this interface routes packets; otherwise .
	 */
	var IsForwardingEnabled(default, never):Bool;
	/**
	 * Gets the maximum transmission unit (MTU) for this network interface.
	 * @return An  value that specifies the MTU.
	 */
	var Mtu(default, never):Int;
	/**
	 * Gets a  value that indicates whether an interface uses Windows Internet Name
	 * Service (WINS).
	 * @return if the interface uses WINS; otherwise, .
	 */
	var UsesWins(default, never):Bool;
}
