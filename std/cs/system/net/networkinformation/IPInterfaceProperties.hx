package cs.system.net.networkinformation;

/** Provides information about network interfaces that support Internet Protocol version 4 (IPv4) or Internet Protocol version 6 (IPv6). */
@:native("System.Net.NetworkInformation.IPInterfaceProperties")
extern class IPInterfaceProperties {
	/**
	 * Gets the anycast IP addresses assigned to this interface.
	 * @return An  that contains the anycast addresses for this interface.
	 */
	var AnycastAddresses(default, never):cs.system.net.networkinformation.IPAddressInformationCollection;
	/**
	 * Gets the addresses of Dynamic Host Configuration Protocol (DHCP) servers for
	 * this interface.
	 * @return An  that contains the address information for DHCP servers, or an empty
	 * array if no servers are found.
	 */
	var DhcpServerAddresses(default, never):cs.system.net.networkinformation.IPAddressCollection;
	/**
	 * Gets the addresses of Domain Name System (DNS) servers for this interface.
	 * @return A  that contains the DNS server addresses.
	 */
	var DnsAddresses(default, never):cs.system.net.networkinformation.IPAddressCollection;
	/**
	 * Gets the Domain Name System (DNS) suffix associated with this interface.
	 * @return A  that contains the DNS suffix for this interface, or  if there is no
	 * DNS suffix for the interface.
	 */
	var DnsSuffix(default, never):String;
	/**
	 * Gets the IPv4 network gateway addresses for this interface.
	 * @return An  that contains the address information for network gateways, or an
	 * empty array if no gateways are found.
	 */
	var GatewayAddresses(default, never):cs.system.net.networkinformation.GatewayIPAddressInformationCollection;
	/**
	 * Gets a  value that indicates whether NetBt is configured to use DNS name
	 * resolution on this interface.
	 * @return if NetBt is configured to use DNS name resolution on this interface;
	 * otherwise, .
	 */
	var IsDnsEnabled(default, never):Bool;
	/**
	 * Gets a  value that indicates whether this interface is configured to
	 * automatically register its IP address information with the Domain Name System
	 * (DNS).
	 * @return if this interface is configured to automatically register a mapping
	 * between its dynamic IP address and static domain names; otherwise, .
	 */
	var IsDynamicDnsEnabled(default, never):Bool;
	/**
	 * Gets the multicast addresses assigned to this interface.
	 * @return An  that contains the multicast addresses for this interface.
	 */
	var MulticastAddresses(default, never):cs.system.net.networkinformation.MulticastIPAddressInformationCollection;
	/**
	 * Gets the unicast addresses assigned to this interface.
	 * @return An  that contains the unicast addresses for this interface.
	 */
	var UnicastAddresses(default, never):cs.system.net.networkinformation.UnicastIPAddressInformationCollection;
	/**
	 * Gets the addresses of Windows Internet Name Service (WINS) servers.
	 * @return An  that contains the address information for WINS servers, or an empty
	 * array if no servers are found.
	 */
	var WinsServersAddresses(default, never):cs.system.net.networkinformation.IPAddressCollection;
	/**
	 * Provides Internet Protocol version 4 (IPv4) configuration data for this network
	 * interface.
	 * @return An  object that contains IPv4 configuration data, or  if no data is
	 * available for the interface.
	 */
	function GetIPv4Properties():cs.system.net.networkinformation.IPv4InterfaceProperties;
	/**
	 * Provides Internet Protocol version 6 (IPv6) configuration data for this network
	 * interface.
	 * @return An  object that contains IPv6 configuration data.
	 */
	function GetIPv6Properties():cs.system.net.networkinformation.IPv6InterfaceProperties;
}
