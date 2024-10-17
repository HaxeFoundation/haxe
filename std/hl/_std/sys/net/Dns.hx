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

@:coreApi
final class Dns {
	public static function resolveSync(name:String):Array<IpAddress> {
		final ipv4 = resolveSyncIpv4Impl(@:privateAccess name.bytes.utf16ToUtf8(0, null));
		return if (ipv4 == -1) {
			[];
		} else {
			[Ipv4Address.fromNetworkOrderInt(ipv4)];
		};
	}

	public static function reverseSync(address:IpAddress):Array<String> {
		switch (address) {
			case V4(ipv4):
				final nameBytes = reverseSyncIpv4Impl(@:privateAccess ipv4.asNetworkOrderInt());
				if (nameBytes != null) {
					final name = @:privateAccess String.fromUTF8(nameBytes);
					return [name];
				}
			case V6(_):
				throw new UnsupportedFamilyException("Hashlink does not support reverse lookup for IPv6 addresses");
		}
		return [];
	}

	public static function getLocalHostname():String {
		final nameBytes = getLocalHostnameImpl();
		if (nameBytes != null) {
			return @:privateAccess String.fromUTF8(nameBytes);
		}
		throw new Sys.SysError("Could not get local hostname");
	}

	@:hlNative("std", "host_resolve")
	private static function resolveSyncIpv4Impl(name:hl.Bytes):Int {
		return -1;
	}

	@:hlNative("std", "host_reverse")
	private static function reverseSyncIpv4Impl(ipv4:Int):hl.Bytes {
		return hl.Bytes.fromAddress(0i64);
	}

	@:hlNative("std", "host_local")
	private static function getLocalHostnameImpl():hl.Bytes {
		return hl.Bytes.fromAddress(0i64);
	}
}
