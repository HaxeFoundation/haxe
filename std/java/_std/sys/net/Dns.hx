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

import haxe.Exception;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;

using Lambda;

@:coreApi
final class Dns {
	public static function resolveSync(name:String):Array<IpAddress> {
		@:nullSafety(Off) var nativeAddrs:Array<InetAddress> = null;

		try {
			nativeAddrs = @:privateAccess Array.ofNative(InetAddress.getAllByName(name));
		} catch (_) {
			return [];
		}

		final addresses:Array<IpAddress> = [];
		for (nativeAddr in nativeAddrs) {
			final ipAddr = IpAddress.tryParse(nativeAddr.getHostAddress());
			if (ipAddr != null) {
				addresses.push(ipAddr);
			}
		}

		return addresses;
	}

	public static function reverseSync(address:IpAddress):Array<String> {
		final nativeAddr = InetAddress.getByName(address.toString());
		return [nativeAddr.getHostName()];
	}

	public static function getLocalHostname():String {
		return InetAddress.getLocalHost().getHostName();
	}
}
