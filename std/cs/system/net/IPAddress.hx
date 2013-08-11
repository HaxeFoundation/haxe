
package cs.system.net;

@:native("System.Net.IPAddress")
extern class IPAddress
{
	/**
	 * Provides a copy of the IPAddress as an array of bytes.
	 */
	public function GetAddressBytes() : haxe.io.BytesData;
	
	/**
	 * Gets the address family of the IP address.
	 */
	public var AddressFamily(default, never) : cs.system.net.sockets.AddressFamily;
	
	/**
	 * Determines whether a string is a valid IP address.
	 * @param	ipString	The string to validate.
	 * @param	address		(out) The IPAddress version of the string.
	 * @return true if ipString is a valid IP address; otherwise, false.
	 */
	public static function TryParse( ipString : String, /* out */ address : IPAddress ) : Bool;
	
	public function ToString() : String;
}
