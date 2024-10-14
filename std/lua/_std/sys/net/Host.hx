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

import haxe.io.Bytes;
import haxe.io.BytesInput;
import lua.NativeStringTools.find;
import lua.lib.luv.Os;
import lua.lib.luv.net.Dns;

@:coreapi
class Host {
	public var host(default, null):String;

	public var ip(get, never):Int;
	public var addresses(default, null):Array<IpAddress>;

	public function new(name:String):Void {
		host = name;
		this.addresses = [];

		final ipv4 = Ipv4Address.tryParse(name);
		if (ipv4 != null) {
			this.addresses.push(ipv4);
			return;
		}

		final infos = lua.lib.luv.net.Dns.getaddrinfo(name, null, null).result;
		if (infos == null) {
			throw "Unrecognized node name";
		}

		lua.PairTools.ipairsEach(infos, (_, addrinfo) -> {
			switch (addrinfo.family) {
				case "inet":
					final ipv4 = Ipv4Address.tryParse(addrinfo.addr);
					this.addresses.push(ipv4);
				case _:
					// Resolving non-IPv4 addresses is not supported yet
			}
		});
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
		return (cast this.ip : Ipv4Address).toString();
	}

	public function reverse():String {
		return Dns.getnameinfo({ip: this.toString()}).result;
	}

	static public function localhost():String {
		return Os.gethostname();
	}
}
