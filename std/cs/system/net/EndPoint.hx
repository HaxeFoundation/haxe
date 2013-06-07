
package cs.system.net;

@:native("System.Net.EndPoint") 
extern class EndPoint {
	/**
	 * Gets the address family to which the endpoint belongs.
	 */
	public var AddressFamily (get, never) : cs.system.net.sockets.AddressFamily;
}