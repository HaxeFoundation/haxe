
package sys.net;

import cs.system.Array;
import cs.system.net.Dns;
import cs.system.net.IPAddress;
import cs.system.net.IPHostEntry;
import cs.system.net.sockets.AddressFamily;
import haxe.io.Bytes;
import haxe.io.BytesInput;

/**
	A given IP host name.
**/
@:coreapi
class Host {
	public var hostEntry(default, null) : IPHostEntry;
	public var ipAddress(default, null) : IPAddress;

	/**
		The actual IP corresponding to the host.
	**/
	public var ip(get, null) : Int;
	private function get_ip() : Int {
		return new BytesInput(Bytes.ofData( ipAddress.GetAddressBytes() )).readInt32();
	}

	/**
		Creates a new Host : the name can be an IP in the form "127.0.0.1" or an host name such as "google.com", in which case
		the corresponding IP address is resolved using DNS. An exception occur if the host name could not be found.
	**/
	public function new( name : String ) : Void {
		hostEntry = Dns.GetHostEntry(name);
		for (i in 0...hostEntry.AddressList.Length) {
			if (hostEntry.AddressList[i].AddressFamily == InterNetwork) {
				ipAddress = hostEntry.AddressList[i];
				break;
			}
		}
	}

	/**
		Returns the IP representation of the host
	**/
	public function toString() : String {
		return ipAddress.ToString();
	}

	/**
		Perform a reverse-DNS query to resolve a host name from an IP.
	**/
	public function reverse() : String {
		return hostEntry.HostName;
	}

	/**
		Returns the local computer host name
	**/
	static public function localhost() : String {
		return Dns.GetHostName();
	}

}
