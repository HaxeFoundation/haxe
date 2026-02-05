package cs.system.net.networkinformation;

/** Provides information about network interfaces that support Internet Protocol version 6 (IPv6). */
@:native("System.Net.NetworkInformation.IPv6InterfaceProperties")
extern class IPv6InterfaceProperties {
	/**
	 * Gets the index of the network interface associated with an Internet Protocol
	 * version 6 (IPv6) address.
	 * @return An  value that contains the index of the network interface for IPv6
	 * address.
	 */
	var Index(default, never):Int;
	/**
	 * Gets the maximum transmission unit (MTU) for this network interface.
	 * @return An  value that specifies the MTU.
	 */
	var Mtu(default, never):Int;
	/**
	 * Gets the scope ID of the network interface associated with an Internet Protocol
	 * version 6 (IPv6) address.
	 * @param scopeLevel The scope level.
	 * @return The scope ID of the network interface associated with an IPv6 address.
	 */
	function GetScopeId(scopeLevel:cs.system.net.networkinformation.ScopeLevel):haxe.Int64;
}
