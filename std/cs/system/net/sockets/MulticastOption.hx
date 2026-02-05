package cs.system.net.sockets;

/** Contains  values used to join and drop multicast groups. */
@:native("System.Net.Sockets.MulticastOption")
extern class MulticastOption {
	/**
	 * Gets or sets the IP address of a multicast group.
	 * @return An  that contains the Internet address of a multicast group.
	 */
	var Group(default, default):cs.system.net.IPAddress;
	/**
	 * Gets or sets the index of the interface that is used to send and receive
	 * multicast packets.
	 * @return An integer that represents the index of a  array element.
	 */
	var InterfaceIndex(default, default):Int;
	/**
	 * Gets or sets the local address associated with a multicast group.
	 * @return An  that contains the local address associated with a multicast group.
	 */
	var LocalAddress(default, default):cs.system.net.IPAddress;
	@:overload(function(group:cs.system.net.IPAddress):Void {})
	@:overload(function(group:cs.system.net.IPAddress, interfaceIndex:Int):Void {})
	function new(group:cs.system.net.IPAddress, mcint:cs.system.net.IPAddress):Void;
}
