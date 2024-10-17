/*
 * Copyright (C)2005-2024 Haxe Foundation
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

import cpp.NativeSocket;

@:coreApi
final class Dns {
	private static function __init__():Void {
		NativeSocket.socket_init();
	}

	public static function resolveSync(name:String):Array<IpAddress> {
		final addresses:Array<IpAddress> = [];
		try {
			final ipv4 = NativeSocket.host_resolve(name);
			addresses.push(Ipv4Address.fromNetworkOrderInt(ipv4));
		} catch (_) {
			// Host not found, ignore
		}
		try {
			final ipv6 = NativeSocket.host_resolve_ipv6(name);
			addresses.push(@:privateAccess Ipv6Address.fromNetworkOrderBytes(ipv6));
		} catch (_) {
			// Host not found, ignore
		}
		return addresses;
	}

	public static function reverseSync(address:IpAddress):Array<String> {
		final name = switch (address) {
			case V4(ipv4):
				NativeSocket.host_reverse(@:privateAccess ipv4.asNetworkOrderInt());
			case V6(ipv6):
				NativeSocket.host_reverse_ipv6(@:privateAccess ipv6.asNetworkOrderBytes());
		}
		return if (name != null && name != "") {
			[name];
		} else {
			[];
		};
	}

	public static function getLocalHostname():String {
		return NativeSocket.host_local();
	}
}
