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

import lua.lib.luv.net.Dns;
import lua.lib.luv.Os;

@:coreapi
class Host {
	public var host(default, null):String;

	public var ip(default, null):Int;

	var _ip:String;

	public function new(name:String):Void {
		host = name;
		if (find(name, "(%d+)%.(%d+)%.(%d+)%.(%d+)").begin != null) {
			_ip = name;
		} else {
			var res = lua.lib.luv.net.Dns.getaddrinfo(name);
			if (res.result == null)
				throw "Unrecognized node name";
			_ip = res.result[1].addr;
			if (_ip == "::1")
				_ip = "127.0.0.0";
		}
		var num = 0;
		for (a in _ip.split(".")) {
			num = num * 256 + lua.Lua.tonumber(a);
		}
		ip = num;
	}

	public function toString():String {
		return _ip;
	}

	public function reverse():String {
		return Dns.getnameinfo({ip: _ip}).result;
	}

	static public function localhost():String {
        return Os.gethostname();
	}
}
