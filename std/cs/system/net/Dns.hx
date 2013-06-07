
package cs.system.net;

import Array;

@:native("System.Net.Dns") 
extern class Dns
{
	/**
	 * Returns the Internet Protocol (IP) addresses for the specified host.
	 */
	public static function GetHostAddresses(hostNameOrAddress : String) : Array<IPAddress>;
	
	/**
	 * Resolves an IP address to an IPHostEntry instance.
	 */
	@:overload(function ( address : IPAddress ) : IPHostEntry { } )
	public static function GetHostEntry( hostNameOrAddress : String ) : IPHostEntry;
	
	/**
	 * Gets the host name of the local computer.
	 */
	public static function GetHostName() : String;
}
