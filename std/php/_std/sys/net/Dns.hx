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

import php.Global.gethostbyaddr;
import php.Global.gethostbynamel;
import php.NativeIndexedArray;
import php.SuperGlobal._SERVER;

using Lambda;

@:coreApi
final class Dns {
	public static function resolveSync(name:String):Array<IpAddress> {
		final addressesStr = gethostbynamel(name);
		if (addressesStr == false) {
			return [];
		}

		final addresses:Array<IpAddress> = [];
		for (addrStr in (cast addressesStr : NativeIndexedArray<String>)) {
			final ipAddr = IpAddress.tryParse(addrStr);
			if (ipAddr != null) {
				addresses.push(ipAddr);
			}
		}

		return addresses;
	}

	public static function reverseSync(address:IpAddress):Array<String> {
		final addressStr = address.toString();
		final reversed = gethostbyaddr(addressStr);
		return if (reversed != false && reversed != addressStr) {
			[reversed];
		} else {
			[];
		};
	}

	public static function getLocalHostname():String {
		return php.Syntax.coalesce(_SERVER['HTTP_HOST'], "localhost");
	}
}
