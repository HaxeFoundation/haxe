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

import sys.net.IpAddress;

@:coreApi
class Host {
	public var host(default, null):String;

	public var ip(get, never):Int;
	public var addresses(default, null):Array<IpAddress>;

	public function new(name:String):Void {
		host = name;
		final ipv4 = host_resolve(@:privateAccess name.bytes.utf16ToUtf8(0, null));
		if (ipv4 == -1) {
			throw new Sys.SysError("Unresolved host " + name);
		}
		this.addresses = [V4(cast ipv4)];
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
		return @:privateAccess String.fromUTF8(host_to_string(ip));
	}

	public function reverse():String {
		return @:privateAccess String.fromUTF8(host_reverse(ip));
	}

	public static function localhost():String {
		return @:privateAccess String.fromUTF8(host_local());
	}

	@:hlNative("std", "host_resolve") static function host_resolve(name:hl.Bytes):Int {
		return 0;
	}

	@:hlNative("std", "host_reverse") static function host_reverse(host:Int):hl.Bytes {
		return null;
	}

	@:hlNative("std", "host_to_string") static function host_to_string(host:Int):hl.Bytes {
		return null;
	}

	@:hlNative("std", "host_local") static function host_local():hl.Bytes {
		return null;
	}
}
