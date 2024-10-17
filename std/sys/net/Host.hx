/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package sys.net;

import haxe.Exception;
import sys.net.IpAddress;

/**
	Stores address information about a given internet host.
**/
class Host {
	/**
		The original provided hostname.
	**/
	public var host(default, null):String;

	/**
		Known IP addresses corresponding to this host. Not in any particular order.
	**/
	public var addresses(default, null):Array<IpAddress>;

	/**
		If any IPv4 address is associated with this host,
		returns an arbitrary one as a big-endian integer.
		Throws otherwise.
	**/
	@:noCompletion
	@:deprecated("Use the `addresses` field instead for better typing and IPv6 support")
	public var ip(get, never):Int;

	private function get_ip():Int {
		for (addr in this.addresses) {
			switch (addr) {
				case V4(ip):
					return @:privateAccess ip.asNetworkOrderInt();
				case _:
			}
		}

		throw new UnsupportedFamilyException("This host does not have any associated IPv4 address");
	}

	/**
		Creates a new `Host`.

		The name can be an IPv4 address (e.g. "127.0.0.1"),
		or an IPv6 address (e.g. "::1"), in which case it will be parsed as such.
		It can also be a hostname (e.g. "google.com"),
		in which case the corresponding IP addresses are resolved using DNS.

		If the hostname could not be found, throws an exception.
	**/
	public function new(name:String) {
		this.host = name;

		final ipAddr = IpAddress.tryParse(name);
		if (ipAddr != null) {
			this.addresses = [ipAddr];
			return;
		}

		final addresses = Dns.resolveSync(name);
		if (addresses.length == 0) {
			throw new DnsException("Could not resolve hostname " + name);
		}

		this.addresses = addresses;
	}

	private function getAddressesSorted(preference:FamilyPreference):Array<IpAddress> {
		final copy = this.addresses.copy();
		copy.sort((a, b) -> switch [preference, a, b] {
			case [PreferIPv4, V4(_), V6(_)]: -1;
			case [PreferIPv4, V6(_), V4(_)]: 1;
			case [PreferIPv6, V6(_), V4(_)]: -1;
			case [PreferIPv6, V4(_), V6(_)]: 1;
			case _: 0;
		});
		return copy;
	}

	/**
		Represents this host and all its IP addresses as a string.
	**/
	public function toString():String {
		final sb = new StringBuf();
		sb.addChar('"'.code);
		sb.add(this.host);
		sb.add("\" (");

		final addresses = this.addresses;
		if (addresses.length == 0) {
			sb.add("no known addresses");
		} else {
			for (i => addr in addresses) {
				sb.add(addr.toString());
				if (i + 1 < addresses.length) {
					sb.add(", ");
				}
			}
		}

		sb.addChar(")".code);
		return sb.toString();
	}

	/**
		Perform a reverse-DNS query to resolve a host name from an IP.
	**/
	@:deprecated("Use `sys.net.Dns.reverseSync` instead")
	public function reverse():String {
		final addresses = this.addresses;
		if (addresses.length == 0) {
			throw new Exception("There are no IP addresses associated with this host");
		}

		final address = addresses[0];
		final reversed = Dns.reverseSync(address);
		if (reversed.length == 0) {
			throw new DnsException('Reverse lookup failed for IP address $address');
		}

		return reversed[0];
	}

	/**
		Returns the local computer hostname.
	**/
	@:deprecated("Use `sys.net.Dns.getLocalHostname` instead")
	public static function localhost():String {
		return Dns.getLocalHostname();
	}
}

@:noDoc @:noCompletion
private enum abstract FamilyPreference(Int) {
	public var PreferIPv4 = 0;
	public var PreferIPv6 = 1;
}
