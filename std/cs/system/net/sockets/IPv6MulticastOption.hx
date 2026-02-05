package cs.system.net.sockets;

/** Contains option values for joining an IPv6 multicast group. */
@:native("System.Net.Sockets.IPv6MulticastOption")
extern class IPv6MulticastOption {
	/**
	 * Gets or sets the IP address of a multicast group.
	 * @return An  that contains the Internet address of a multicast group.
	 */
	var Group(default, default):cs.system.net.IPAddress;
	/**
	 * Gets or sets the interface index that is associated with a multicast group.
	 * @return A  value that specifies the address of the interface.
	 */
	var InterfaceIndex(default, default):haxe.Int64;
	@:overload(function(group:cs.system.net.IPAddress):Void {})
	function new(group:cs.system.net.IPAddress, ifindex:haxe.Int64):Void;
}
