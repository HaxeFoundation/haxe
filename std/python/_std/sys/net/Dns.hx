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
import haxe.extern.EitherType;
import python.Syntax;
import python.Tuple;

@:coreApi
final class Dns {
	public static function resolveSync(name:String):Array<IpAddress> {
		try {
			final addresses:Array<IpAddress> = [];
			for (addrinfo in PythonSocket.getaddrinfo(name, null, 0, 0, 0, 0)) {
				final family:Int = addrinfo[0];
				final sockAddr:InetSockAddr = addrinfo[4];
				final ipStr:String = (cast sockAddr : Dynamic)[0];
				if (!Lambda.exists(addresses, a -> a.toString() == ipStr)) {
					if (family == PythonSocket.AF_INET) {
						final ipv4 = Ipv4Address.tryParse(ipStr);
						if (ipv4 != null) {
							addresses.push(ipv4);
						}
					} else if (family == PythonSocket.AF_INET6) {
						final ipv6 = Ipv6Address.tryParse(ipStr);
						if (ipv6 != null) {
							addresses.push(ipv6);
						}
					}
				}
			}
			return addresses;
		} catch (e:Exception) {
			return [];
		}
	}

	public static function reverseSync(address:IpAddress):Array<String> {
		final nameInfo = switch (address) {
			case V4(ipv4):
				PythonSocket.getnameinfo(Syntax.tuple(ipv4.toString(), 0), 0);
			case V6(ipv6):
				PythonSocket.getnameinfo(Syntax.tuple(ipv6.toString(), 0, 0, 0), 0);
		};

		final reversed = nameInfo[0];
		return if (reversed != address.toString()) {
			[reversed];
		} else {
			[];
		};
	}

	public static function getLocalHostname():String {
		return PythonSocket.gethostname();
	}
}

private typedef Inet4SockAddr = Tuple2<String, Int>;
private typedef Inet6SockAddr = Tuple4<String, Int, Int, Int>;
private typedef InetSockAddr = EitherType<Inet4SockAddr, Inet6SockAddr>;

@:pythonImport("socket")
private extern class PythonSocket {
	public static final AF_INET:Int;
	public static final AF_INET6:Int;
	public static function getaddrinfo(host:String, ?port:EitherType<Int, String>, family:Int, type:Int, proto:Int,
		flags:Int):Array<Tuple5<Int, Int, Int, String, InetSockAddr>>;
	public static function gethostname():String;
	public static function getnameinfo(sockaddr:InetSockAddr, flags:Int):Tuple2<String, String>;
}
