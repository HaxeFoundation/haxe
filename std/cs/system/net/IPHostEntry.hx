package cs.system.net;

/** Provides a container class for Internet host address information. */
@:native("System.Net.IPHostEntry")
extern class IPHostEntry {
	/**
	 * Gets or sets a list of IP addresses that are associated with a host.
	 * @return An array of type  that contains IP addresses that resolve to the host
	 * names that are contained in the  property.
	 */
	var AddressList(default, default):cs.NativeArray<cs.system.net.IPAddress>;
	/**
	 * Gets or sets a list of aliases that are associated with a host.
	 * @return An array of strings that contain DNS names that resolve to the IP
	 * addresses in the  property.
	 */
	var Aliases(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets the DNS name of the host.
	 * @return A string that contains the primary host name for the server.
	 */
	var HostName(default, default):String;
	function new():Void;
}
