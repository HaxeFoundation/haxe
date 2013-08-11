
package cs.system.net;

@:native("System.Net.IPHostEntry") 
extern class IPHostEntry
{
	/**
	 * Gets or sets a list of IP addresses that are associated with a host.
	 */
	public var AddressList(default, default) : NativeArray<IPAddress>;
	
	/**
	 * Gets or sets a list of aliases that are associated with a host.
	 */
	public var Aliases(default, default) : NativeArray<String>;
	
	/**
	 * Gets or sets the DNS name of the host.
	 */
	public var HostName (default, default) : String;
}
