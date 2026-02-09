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

class Host {
	public var host(default, null):String;
	public var ip(default, null):Int;

	private var _ipAddress:Dynamic; // System.Net.IPAddress

	public function new(name:String):Void {
		host = name;
		resolve(name);
	}

	private function resolve(name:String):Void {
		try {
			// Try to parse as IP address first
			var ipAddr:Dynamic = cs.Syntax.code("System.Net.IPAddress.Parse({0})", name);
			_ipAddress = ipAddr;
			setIpFromAddress(ipAddr);
		} catch (e:Dynamic) {
			// Not an IP address, resolve hostname
			try {
				var addresses:Dynamic = cs.Syntax.code("System.Net.Dns.GetHostAddresses({0})", name);
				if (cs.Syntax.code("((System.Net.IPAddress[]){0}).Length", addresses) > 0) {
					// Get the first IPv4 address if available
					var len:Int = cs.Syntax.code("((System.Net.IPAddress[]){0}).Length", addresses);
					for (i in 0...len) {
						var addr:Dynamic = cs.Syntax.code("((System.Net.IPAddress[]){0})[{1}]", addresses, i);
						var family:Int = cs.Syntax.code("(int)((System.Net.IPAddress){0}).AddressFamily", addr);
						// AddressFamily.InterNetwork = 2 (IPv4)
						if (family == 2) {
							_ipAddress = addr;
							setIpFromAddress(addr);
							return;
						}
					}
					// Fall back to first address
					_ipAddress = cs.Syntax.code("((System.Net.IPAddress[]){0})[0]", addresses);
					setIpFromAddress(_ipAddress);
				} else {
					throw "Could not resolve host: " + name;
				}
			} catch (ex:Dynamic) {
				throw "Could not resolve host: " + name;
			}
		}
	}

	private function setIpFromAddress(addr:Dynamic):Void {
		var bytes:Dynamic = cs.Syntax.code("((System.Net.IPAddress){0}).GetAddressBytes()", addr);
		var len:Int = cs.Syntax.code("((byte[]){0}).Length", bytes);
		if (len >= 4) {
			// network byte order (big endian)
			var b0:Int = cs.Syntax.code("(int)((byte[]){0})[0]", bytes);
			var b1:Int = cs.Syntax.code("(int)((byte[]){0})[1]", bytes);
			var b2:Int = cs.Syntax.code("(int)((byte[]){0})[2]", bytes);
			var b3:Int = cs.Syntax.code("(int)((byte[]){0})[3]", bytes);
			this.ip = b3 | (b2 << 8) | (b1 << 16) | (b0 << 24);
		}
	}

	public function toString():String {
		if (_ipAddress != null) {
			return cs.Syntax.code("{0}.ToString()", _ipAddress);
		}
		return host;
	}

	public function reverse():String {
		try {
			var entry:Dynamic = cs.Syntax.code("System.Net.Dns.GetHostEntry({0})", _ipAddress);
			return cs.Syntax.code("{0}.HostName", entry);
		} catch (e:Dynamic) {
			return host;
		}
	}

	public static function localhost():String {
		return cs.Syntax.code("System.Net.Dns.GetHostName()");
	}
}
