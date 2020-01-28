package asyncio.net;

import haxe.errors.NotImplemented;
import haxe.io.Bytes;

/**
	Represents a resolved IP address.
**/
@:using(asyncio.net.Ip.IpTools)
enum Ip {
	/**
		32-bit IPv4 address. As an example, the IP address `127.0.0.1` is
		represented as `Ipv4(0x7F000001)`.
	**/
	Ipv4(raw:Int);

	/**
		128-bit IPv6 address.
	**/
	Ipv6(raw:Bytes);
}

class IpTools {
	/**
		String representation of `ip`.
		Examples:
		- IPv4: "192.168.0.1"
		- IPv6: "::ffff:c0a8:1"
	**/
	static public function toString(ip:Ip):String {
		throw new NotImplemented();
	}

	/**
		String representation of `ip`.
		Examples:
		- IPv4: "192.168.0.1"
		- IPv6: "0000:0000:0000:0000:0000:ffff:c0a8:1"
	**/
	static public function toFullString(ip:Ip):String {
		throw new NotImplemented();
	}

	/**
		Parse a string representation of an IP address.

		Throws an exception if provided string does not represent a valid IP address.
	**/
	static public function parse(ip:String):Ip {
		throw new NotImplemented();
	}

	/**
		Check if `str` contains a valid IPv6 or IPv4 address.
	**/
	static public function isIp(str:String):Bool {
		throw new NotImplemented();
	}

	/**
		Check if `str` contains a valid IPv4 address.
	**/
	static public function isIpv4(str:String):Bool {
		throw new NotImplemented();
	}

	/**
		Check if `str` contains a valid IPv6 address.
	**/
	static public function isIpv6(str:String):Bool {
		throw new NotImplemented();
	}

	/**
		Check if `a` and `b` contain the same IP address.
	**/
	static public function equals(a:Ip, b:Ip):Bool {
		throw new NotImplemented();
	}
}