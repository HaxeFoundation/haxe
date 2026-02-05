package cs.system.net.networkinformation;

/** Provides information about the network connectivity of the local computer. */
@:native("System.Net.NetworkInformation.IPGlobalProperties")
extern class IPGlobalProperties {
	/**
	 * Gets the Dynamic Host Configuration Protocol (DHCP) scope name.
	 * @return A  instance that contains the computer's DHCP scope name.
	 */
	var DhcpScopeName(default, never):String;
	/**
	 * Gets the domain in which the local computer is registered.
	 * @return A  instance that contains the computer's domain name. If the computer
	 * does not belong to a domain, returns .
	 */
	var DomainName(default, never):String;
	/**
	 * Gets the host name for the local computer.
	 * @return A  instance that contains the computer's NetBIOS name.
	 */
	var HostName(default, never):String;
	/**
	 * Gets a  value that specifies whether the local computer is acting as a Windows
	 * Internet Name Service (WINS) proxy.
	 * @return if the local computer is a WINS proxy; otherwise, .
	 */
	var IsWinsProxy(default, never):Bool;
	/**
	 * Gets the Network Basic Input/Output System (NetBIOS) node type of the local
	 * computer.
	 * @return A  value.
	 */
	var NodeType(default, never):cs.system.net.networkinformation.NetBiosNodeType;
	/**
	 * Gets an object that provides information about the local computer's network
	 * connectivity and traffic statistics.
	 * @return A  object that contains information about the local computer.
	 */
	static function GetIPGlobalProperties():cs.system.net.networkinformation.IPGlobalProperties;
	/**
	 * Begins an asynchronous request to retrieve the stable unicast IP address table
	 * on the local computer.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous request.
	 */
	function BeginGetUnicastAddresses(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Ends a pending asynchronous request to retrieve the stable unicast IP address
	 * table on the local computer.
	 * @param asyncResult An  that references the asynchronous request.
	 * @return An  that stores state information and any user defined data for this
	 * asynchronous operation.
	 */
	function EndGetUnicastAddresses(asyncResult:cs.system.IAsyncResult):cs.system.net.networkinformation.UnicastIPAddressInformationCollection;
	/**
	 * Returns information about the Internet Protocol version 4 (IPv4) and IPv6
	 * Transmission Control Protocol (TCP) connections on the local computer.
	 * @return A  array that contains objects that describe the active TCP connections,
	 * or an empty array if no active TCP connections are detected.
	 */
	function GetActiveTcpConnections():cs.NativeArray<cs.system.net.networkinformation.TcpConnectionInformation>;
	/**
	 * Returns endpoint information about the Internet Protocol version 4 (IPv4) and
	 * IPv6 Transmission Control Protocol (TCP) listeners on the local computer.
	 * @return A  array that contains objects that describe the active TCP listeners,
	 * or an empty array, if no active TCP listeners are detected.
	 */
	function GetActiveTcpListeners():cs.NativeArray<cs.system.net.IPEndPoint>;
	/**
	 * Returns information about the Internet Protocol version 4 (IPv4) and IPv6 User
	 * Datagram Protocol (UDP) listeners on the local computer.
	 * @return An  array that contains objects that describe the UDP listeners, or an
	 * empty array if no UDP listeners are detected.
	 */
	function GetActiveUdpListeners():cs.NativeArray<cs.system.net.IPEndPoint>;
	/**
	 * Provides Internet Control Message Protocol (ICMP) version 4 statistical data for
	 * the local computer.
	 * @return An  object that provides ICMP version 4 traffic statistics for the local
	 * computer.
	 */
	function GetIcmpV4Statistics():cs.system.net.networkinformation.IcmpV4Statistics;
	/**
	 * Provides Internet Control Message Protocol (ICMP) version 6 statistical data for
	 * the local computer.
	 * @return An  object that provides ICMP version 6 traffic statistics for the local
	 * computer.
	 */
	function GetIcmpV6Statistics():cs.system.net.networkinformation.IcmpV6Statistics;
	/**
	 * Provides Internet Protocol version 4 (IPv4) statistical data for the local
	 * computer.
	 * @return An  object that provides IPv4 traffic statistics for the local computer.
	 */
	function GetIPv4GlobalStatistics():cs.system.net.networkinformation.IPGlobalStatistics;
	/**
	 * Provides Internet Protocol version 6 (IPv6) statistical data for the local
	 * computer.
	 * @return An  object that provides IPv6 traffic statistics for the local computer.
	 */
	function GetIPv6GlobalStatistics():cs.system.net.networkinformation.IPGlobalStatistics;
	/**
	 * Provides Transmission Control Protocol/Internet Protocol version 4 (TCP/IPv4)
	 * statistical data for the local computer.
	 * @return A  object that provides TCP/IPv4 traffic statistics for the local
	 * computer.
	 */
	function GetTcpIPv4Statistics():cs.system.net.networkinformation.TcpStatistics;
	/**
	 * Provides Transmission Control Protocol/Internet Protocol version 6 (TCP/IPv6)
	 * statistical data for the local computer.
	 * @return A  object that provides TCP/IPv6 traffic statistics for the local
	 * computer.
	 */
	function GetTcpIPv6Statistics():cs.system.net.networkinformation.TcpStatistics;
	/**
	 * Provides User Datagram Protocol/Internet Protocol version 4 (UDP/IPv4)
	 * statistical data for the local computer.
	 * @return A  object that provides UDP/IPv4 traffic statistics for the local
	 * computer.
	 */
	function GetUdpIPv4Statistics():cs.system.net.networkinformation.UdpStatistics;
	/**
	 * Provides User Datagram Protocol/Internet Protocol version 6 (UDP/IPv6)
	 * statistical data for the local computer.
	 * @return A  object that provides UDP/IPv6 traffic statistics for the local
	 * computer.
	 */
	function GetUdpIPv6Statistics():cs.system.net.networkinformation.UdpStatistics;
	/**
	 * Retrieves the stable unicast IP address table on the local computer.
	 * @return A  that contains a list of stable unicast IP addresses on the local
	 * computer.
	 */
	function GetUnicastAddresses():cs.system.net.networkinformation.UnicastIPAddressInformationCollection;
	/**
	 * Retrieves the stable unicast IP address table on the local computer as an
	 * asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function GetUnicastAddressesAsync():cs.system.threading.tasks.Task_1<cs.system.net.networkinformation.UnicastIPAddressInformationCollection>;
}
