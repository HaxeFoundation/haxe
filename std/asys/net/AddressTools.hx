package asys.net;

import haxe.io.Bytes;
import asys.net.IpFamily;

/**
	Methods for converting to and from `Address` instances.
**/
class AddressTools {
	static final v4re = {
		final v4seg = "(?:[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])";
		final v4str = '${v4seg}\\.${v4seg}\\.${v4seg}\\.${v4seg}';
		new EReg('^${v4str}$$', "");
	};

	static final v6re = {
		final v4seg = "(?:[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])";
		final v4str = '${v4seg}\\.${v4seg}\\.${v4seg}\\.${v4seg}';
		final v6seg = "(?:[0-9a-fA-F]{1,4})";
		new EReg("^("
			+ '(?:${v6seg}:){7}(?:${v6seg}|:)|'
			+ '(?:${v6seg}:){6}(?:${v4str}|:${v6seg}|:)|'
			+ '(?:${v6seg}:){5}(?::${v4str}|(:${v6seg}){1,2}|:)|'
			+ '(?:${v6seg}:){4}(?:(:${v6seg}){0,1}:${v4str}|(:${v6seg}){1,3}|:)|'
			+ '(?:${v6seg}:){3}(?:(:${v6seg}){0,2}:${v4str}|(:${v6seg}){1,4}|:)|'
			+ '(?:${v6seg}:){2}(?:(:${v6seg}){0,3}:${v4str}|(:${v6seg}){1,5}|:)|'
			+ '(?:${v6seg}:){1}(?:(:${v6seg}){0,4}:${v4str}|(:${v6seg}){1,6}|:)|'
			+ '(?::((?::${v6seg}){0,5}:${v4str}|(?::${v6seg}){1,7}|:))'
			+ ")$", // "(%[0-9a-zA-Z]{1,})?$", // TODO: interface not supported
			"");
	};

	/**
		Returns the IP address representing all hosts for the given IP family.

		- For IPv4, the address is `0.0.0.0`.
		- For IPv6, the address is `::`.
	**/
	public static function all(family:IpFamily):Address {
		return (switch (family) {
			case Ipv4: Ipv4(0);
			case Ipv6: Ipv6(Bytes.ofHex("00000000000000000000000000000000"));
		});
	}

	/**
		Returns the IP address representing the local hosts for the given IP family.

		- For IPv4, the address is `127.0.0.1`.
		- For IPv6, the address is `::1`.
	**/
	public static function localhost(family:IpFamily):Address {
		return (switch (family) {
			case Ipv4: Ipv4(0x7F000001);
			case Ipv6: Ipv6(Bytes.ofHex("00000000000000000000000000000001"));
		});
	}

	/**
		Converts an `Address` to a `String`.

		- IPv4 addresses are represented with the dotted quad format, e.g.
			`192.168.0.1`.
		- IPv6 addresses are represented with the standard lowercased hexadecimal
			representation, with `::` used to mark a long stretch of zeros.
	**/
	public static function toString(address:Address):String {
		return (switch (address) {
			case Ipv4(ip):
				'${ip >>> 24}.${(ip >> 16) & 0xFF}.${(ip >> 8) & 0xFF}.${ip & 0xFF}';
			case Ipv6(ip):
				var groups = [for (i in 0...8) (ip.get(i * 2) << 8) | ip.get(i * 2 + 1)];
				var longestRun = -1;
				var longestPos = -1;
				for (i in 0...8) {
					if (groups[i] != 0)
						continue;
					var run = 1;
					// TODO: skip if the longest run cannot be beaten
					for (j in i + 1...8) {
						if (groups[j] != 0)
							break;
						run++;
					}
					if (run > longestRun) {
						longestRun = run;
						longestPos = i;
					}
				}
				inline function hex(groups:Array<Int>):String {
					return groups.map(value -> StringTools.hex(value, 1).toLowerCase()).join(":");
				}
				if (longestRun > 1) {
					hex(groups.slice(0, longestPos)) + "::" + hex(groups.slice(longestPos + longestRun));
				} else {
					hex(groups);
				}
		});
	}

	/**
		Returns `true` if `address` represents a valid IPv4 or IPv6 address.
	**/
	public static function isIp(address:String):Bool {
		return isIpv4(address) || isIpv6(address);
	}

	/**
		Returns `true` if `address` represents a valid IPv4 address.
	**/
	public static function isIpv4(address:String):Bool {
		return v4re.match(address);
	}

	/**
		Returns `true` if `address` represents a valid IPv6 address.
	**/
	public static function isIpv6(address:String):Bool {
		return v6re.match(address);
	}

	/**
		Tries to convert the `String` `address` to an `Address` instance. Returns
		the parsed `Address` or `null` if `address` does not represent a valid IP
		address.
	**/
	public static function toIp(address:String):Null<Address> {
		var ipv4 = toIpv4(address);
		return ipv4 != null ? ipv4 : toIpv6(address);
	}

	/**
		Tries to convert the `String` `address` to an IPv4 `Address` instance.
		Returns the parsed `Address` or `null` if `address` does not represent a
		valid IPv4 address.
	**/
	public static function toIpv4(address:String):Null<Address> {
		if (!isIpv4(address))
			return null;
		var components = address.split(".").map(Std.parseInt);
		return Ipv4((components[0] << 24) | (components[1] << 16) | (components[2] << 8) | components[3]);
	}

	/**
		Tries to convert the `String` `address` to an IPv6 `Address` instance.
		Returns the parsed `Address` or `null` if `address` does not represent a
		valid IPv6 address.
	**/
	public static function toIpv6(address:String):Null<Address> {
		if (!isIpv6(address))
			return null;
		var buffer = Bytes.alloc(16);
		buffer.fill(0, 16, 0);
		function parse(component:String, res:Int):Void {
			var value = Std.parseInt('0x0$component');
			buffer.set(res, value >> 8);
			buffer.set(res + 1, value & 0xFF);
		}
		var stretch = address.split("::");
		var components = stretch[0].split(":");
		for (i in 0...components.length)
			parse(components[i], i * 2);
		if (stretch.length > 1) {
			var end = 16;
			components = stretch[1].split(":");
			if (isIpv4(components[components.length - 1])) {
				end -= 4;
				var ip = components.pop().split(".").map(Std.parseInt);
				for (i in 0...4)
					buffer.set(end + i, ip[i]);
			}
			end -= components.length * 2;
			for (i in 0...components.length)
				parse(components[i], end + i);
		}
		return Ipv6(buffer);
	}

	/**
		Returns the IPv6 version of the given `address`. IPv6 addresses are
		returned unmodified, IPv4 addresses are mapped to IPv6 using the
		`:ffff:0:0/96` IPv4 transition prefix.

		```haxe
		"127.0.0.1".toIpv4().mapToIpv6().toString(); // ::ffff:7f00:1
		```
	**/
	public static function mapToIpv6(address:Address):Address {
		return (switch (address) {
			case Ipv4(ip):
				var buffer = Bytes.alloc(16);
				buffer.set(10, 0xFF);
				buffer.set(11, 0xFF);
				buffer.set(12, ip >>> 24);
				buffer.set(13, (ip >> 16) & 0xFF);
				buffer.set(14, (ip >> 8) & 0xFF);
				buffer.set(15, ip & 0xFF);
				Ipv6(buffer);
			case _:
				address;
		});
	}

	/**
		Returns `true` if `a` and `b` are the same IP address.

		If `ipv6mapped` is `true`, bot `a` and `b` are mapped to IPv6 (using
		`mapToIpv6`) before the comparison.
	**/
	public static function equals(a:Address, b:Address, ?ipv6mapped:Bool = false):Bool {
		if (ipv6mapped) {
			return (switch [mapToIpv6(a), mapToIpv6(b)] {
				case [Ipv6(a), Ipv6(b)]: a.compare(b) == 0;
				case _: false; // cannot happen?
			});
		}
		return (switch [a, b] {
			case [Ipv4(a), Ipv4(b)]: a == b;
			case [Ipv6(a), Ipv6(b)]: a.compare(b) == 0;
			case _: false;
		});
	}
}
