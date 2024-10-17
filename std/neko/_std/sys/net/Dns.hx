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

import haxe.exceptions.NotImplementedException;
import neko.Lib;
import neko.NativeString;

@:coreApi
@:keepInit
final class Dns {
	private static function __init__():Void {
		Dns.resolveSyncIpv4Impl = cast Lib.load("std", "host_resolve", 1);
		Dns.reverseSyncIpv4Impl = cast Lib.load("std", "host_reverse", 1);
		Dns.getLocalHostnameImpl = cast Lib.load("std", "host_local", 0);
	}

	public static function resolveSync(name:String):Array<IpAddress> {
		final ipv4 = resolveSyncIpv4Impl(NativeString.ofString(name));
		final address = Ipv4Address.fromNetworkOrderInt(ipv4);
		return [address];
	}

	private static dynamic function resolveSyncIpv4Impl(name:NativeString):Int {
		throw new NotImplementedException("Neko function was not loaded");
	}

	public static function reverseSync(address:IpAddress):Array<String> {
		return switch (address) {
			case V4(addr):
				final ipv4 = addr.asNetworkOrderInt();
				final reversed = reverseSyncIpv4Impl(ipv4);
				[NativeString.toString(reversed)];
			case _:
				throw new UnsupportedFamilyException("Neko cannot reverse lookup IPv6 addresses");
		};
	}

	private static dynamic function reverseSyncIpv4Impl(ipv4:Int):NativeString {
		throw new NotImplementedException("Neko function was not loaded");
	}

	public static function getLocalHostname():String {
		return NativeString.toString(getLocalHostnameImpl());
	}

	private static dynamic function getLocalHostnameImpl():NativeString {
		throw new NotImplementedException("Neko function was not loaded");
	}
}
