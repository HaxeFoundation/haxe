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

import cpp.NativeSocket;

@:coreApi
class Host {
	public var host(default, null):String;

	public var ip(get, never):Int;

	private var ipv6(default, null):haxe.io.BytesData;

	public var addresses(default, null):Array<IpAddress>;

	public function new(name:String):Void {
		host = name;
		this.addresses = [];
		try {
			final ip = NativeSocket.host_resolve(name);
			this.addresses.push(V4(cast ip));
		} catch (e:Dynamic) {
			final ipv6 = NativeSocket.host_resolve_ipv6(name);
			throw new UnsupportedFamilyException("ipv6 support currently disabled");
		}
	}

	@:noDoc @:noCompletion
	private function get_ip():Int {
		for (addr in this.addresses) {
			switch (addr) {
				case V4(ip):
					return cast ip;
				case _:
			}
		}
		throw new UnsupportedFamilyException("This host does not support IPv4");
	}

	public function toString():String {
		return ipv6 == null ? NativeSocket.host_to_string(ip) : NativeSocket.host_to_string_ipv6(ipv6);
	}

	public function reverse():String {
		return ipv6 == null ? NativeSocket.host_reverse(ip) : NativeSocket.host_reverse_ipv6(ipv6);
	}

	public static function localhost():String {
		return NativeSocket.host_local();
	}

	static function __init__():Void {
		NativeSocket.socket_init();
	}
}
