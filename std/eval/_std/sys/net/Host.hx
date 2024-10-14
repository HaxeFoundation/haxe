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
import sys.net.UnsupportedFamilyException;

class Host {
	public var host(default, null):String;
	public var ip(get, never):Int;
	public var addresses(default, null):Array<IpAddress>;

	public function new(name:String) {
		host = name;
		init(resolve(name));
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

	public function toString() {
		return hostToString(ip);
	}

	public function reverse() {
		return hostReverse(ip);
	}

	function init(ip:Int) {
		this.addresses = [V4(cast ip)];
	}

	extern static public function localhost():String;

	extern static function hostReverse(ip:Int):String;

	extern static function hostToString(ip:Int):String;

	extern static function resolve(name:String):Int;
}
