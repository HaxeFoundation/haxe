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

package cpp.uv;

using cpp.uv.UV;

/**
	Network address families.
**/
enum AddressFamily {
	UNSPEC;
	INET;
	INET6;
	OTHER(i:Int);
}

/**
	Socket types.
**/
enum SocketType {
	STREAM;
	DGRAM;
	RAW;
	OTHER(i:Int);
}

@:headerCode('#include "uv.h"')
class SockAddr {
	/** Extracts the port in a network address. */
	public var port(get,never):Null<Int>;

	@:allow(cpp.uv)
	final storage:RawPointer<SockaddrStorage>;

	@:allow(cpp.uv)
	function new() {
		storage = SockaddrStorage.create();
		cpp.vm.Gc.setFinalizer(this, Function.fromStaticFunction(finalizer));
	}

	static function finalizer(addr:SockAddr) {
		Pointer.fromRaw(addr.storage).destroy();
	}

	/**
		Converts an ip and port number to an IPv4 struct sockaddr.
	**/
	static public function ipv4(ip:String, port:Int):SockAddr {
		var addr = new SockAddr();
		UV.ip4_addr(ip, port, cast addr.storage).resolve();
		return addr;
	}

	/**
		Converts an ip and port number to an IPv6 struct sockaddr.
	**/
	static public function ipv6(ip:String, port:Int):SockAddr {
		var addr = new SockAddr();
		UV.ip6_addr(ip, port, cast addr.storage).resolve();
		return addr;
	}

	/**
		Converts a network address to a string.
	**/
	public function toString():String {
		return switch port {
			case null: name();
			case p: '${name()}:$p';
		}
	}

	// @:allow(cpp.uv)
	// static function ofSockaddrStorage(addr:Null<CSockaddrStorageStar>):Null<SockAddr> {
	// 	if(addr == null)
	// 		return null;
	// 	var buf = new Bytes(256);
	// 	var size = I64.ofInt(256);
	// 	return switch addr.sockaddr_storage_ss_family().address_family_of_af() {
	// 		case INET:
	// 			UV.ip4_name(addr.sockaddr_in_of_storage(), buf, size);
	// 			Ip4Addr(buf.fromUTF8(), addr.sockaddr_in_of_storage().sockaddr_in_sin_port());
	// 		case INET6:
	// 			UV.ip6_name(addr.sockaddr_in6_of_storage(), buf, size);
	// 			Ip6Addr(buf.fromUTF8(), addr.sockaddr_in6_of_storage().sockaddr_in6_sin6_port());
	// 		case _:
	// 			null;
	// 	}
	// }

	/**
		Extracts ip address as a string.
	**/
	public function name():String {
		var buf:Pointer<Char> = Stdlib.malloc(256);
		var result = switch Pointer.fromRaw(storage).ref.ss_family {
			case AF_INET:
				UV.ip4_name(cast storage, buf.raw, 256);
			case AF_INET6:
				UV.ip6_name(cast storage, buf.raw, 256);
			case _:
				Stdlib.free(buf);
				return null;
		}
		if(result < 0) {
			Stdlib.free(buf);
			result.throwErr();
		}
		return new String(untyped buf.raw); //TODO: is this ok?
	}

	function get_port():Null<Int> {
		return switch Pointer.fromRaw(storage).ref.ss_family {
			case AF_INET:
				Pointer.fromRaw((cast storage:RawPointer<SockaddrIn>)).ref.sin_port;
			case AF_INET6:
				Pointer.fromRaw((cast storage:RawPointer<SockaddrIn6>)).ref.sin6_port;
			case _:
				null;
		}
	}
}