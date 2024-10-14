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

import java.net.InetAddress;

class Host {
	public var host(default, null):String;
	public var ip(get, never):Int;
	public var addresses(default, null):Array<IpAddress>;

	@:allow(sys.net) private var wrapped:InetAddress;

	public function new(name:String):Void {
		host = name;
		try {
			this.wrapped = InetAddress.getByName(name);
		} catch (e:Dynamic) {
			throw e;
		}

		this.addresses = [];
		if (Std.isOfType(this.wrapped, java.net.Inet4Address)) {
			final rawIp = this.wrapped.getAddress();
			final ipv4 = new Ipv4Address(cast(rawIp[3], Int), cast(rawIp[2], Int), cast(rawIp[1], Int), cast(rawIp[0], Int));
			this.addresses.push(ipv4);
		} else {
			throw new UnsupportedFamilyException("Resolving IPv6 addresses it not supported yet");
		}
	}

	@:noDoc @:noCompletion
	private function get_ip():Int {
		for (addr in this.addresses) {
			switch (addr) {
				case V4(ip):
					return cast ip;
			}
		}
		throw new UnsupportedFamilyException("This host does not support IPv4");
	}

	public function toString():String {
		return wrapped.getHostAddress();
	}

	public function reverse():String {
		return wrapped.getHostName();
	}

	public static function localhost():String {
		try {
			return InetAddress.getLocalHost().getHostName();
		} catch (e:Dynamic)
			throw e;
	}
}
