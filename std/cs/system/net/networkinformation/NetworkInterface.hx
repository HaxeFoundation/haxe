package cs.system.net.networkinformation;

/** Provides configuration and statistical information for a network interface. */
@:native("System.Net.NetworkInformation.NetworkInterface")
extern class NetworkInterface {
	/**
	 * Gets the index of the IPv6 loopback interface.
	 * @return The index for the IPv6 loopback interface.
	 */
	static var IPv6LoopbackInterfaceIndex(default, never):Int;
	/**
	 * Gets the index of the IPv4 loopback interface.
	 * @return A  that contains the index for the IPv4 loopback interface.
	 */
	static var LoopbackInterfaceIndex(default, never):Int;
	/**
	 * Gets the description of the interface.
	 * @return A  that describes this interface.
	 */
	var Description(default, never):String;
	/**
	 * Gets the identifier of the network adapter.
	 * @return A  that contains the identifier.
	 */
	var Id(default, never):String;
	/**
	 * Gets a  value that indicates whether the network interface is set to only
	 * receive data packets.
	 * @return if the interface only receives network traffic; otherwise, .
	 */
	var IsReceiveOnly(default, never):Bool;
	/**
	 * Gets the name of the network adapter.
	 * @return A  that contains the adapter name.
	 */
	var Name(default, never):String;
	/**
	 * Gets the interface type.
	 * @return An  value that specifies the network interface type.
	 */
	var NetworkInterfaceType(default, never):cs.system.net.networkinformation.NetworkInterfaceType;
	/**
	 * Gets the current operational state of the network connection.
	 * @return One of the  values.
	 */
	var OperationalStatus(default, never):cs.system.net.networkinformation.OperationalStatus;
	/**
	 * Gets the speed of the network interface.
	 * @return A  value that specifies the speed in bits per second.
	 */
	var Speed(default, never):haxe.Int64;
	/**
	 * Gets a  value that indicates whether the network interface is enabled to receive
	 * multicast packets.
	 * @return if the interface receives multicast packets; otherwise, .
	 */
	var SupportsMulticast(default, never):Bool;
	/**
	 * Returns objects that describe the network interfaces on the local computer.
	 * @return A  array that contains objects that describe the available network
	 * interfaces, or an empty array if no interfaces are detected.
	 */
	static function GetAllNetworkInterfaces():cs.NativeArray<cs.system.net.networkinformation.NetworkInterface>;
	/**
	 * Indicates whether any network connection is available.
	 * @return if a network connection is available; otherwise, .
	 */
	static function GetIsNetworkAvailable():Bool;
	/**
	 * Returns an object that describes the configuration of this network interface.
	 * @return An  object that describes this network interface.
	 */
	function GetIPProperties():cs.system.net.networkinformation.IPInterfaceProperties;
	/**
	 * Gets the IP statistics for this  instance.
	 * @return The IP statistics.
	 */
	function GetIPStatistics():cs.system.net.networkinformation.IPInterfaceStatistics;
	/**
	 * Gets the IPv4 statistics for this  instance.
	 * @return An  object.
	 */
	function GetIPv4Statistics():cs.system.net.networkinformation.IPv4InterfaceStatistics;
	/**
	 * Returns the Media Access Control (MAC) or physical address for this adapter.
	 * @return A  object that contains the physical address.
	 */
	function GetPhysicalAddress():cs.system.net.networkinformation.PhysicalAddress;
	/**
	 * Gets a  value that indicates whether the interface supports the specified
	 * protocol.
	 * @param networkInterfaceComponent A  value.
	 * @return if the specified protocol is supported; otherwise, .
	 */
	function Supports(networkInterfaceComponent:cs.system.net.networkinformation.NetworkInterfaceComponent):Bool;
}
