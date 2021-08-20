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

package hl.uv;

import hl.types.ArrayObj;
import hl.uv.SockAddr;
import hl.uv.Request;

enum abstract AddrInfoFlags(Int) from Int to Int {
	/** Socket address is intended for `bind'. */
	var AI_PASSIVE = 1;
	/** Request for canonical name. */
	var AI_CANONNAME = 2;
	/** Don't use name resolution. */
	var AI_NUMERICHOST = 4;
	/** IPv4 mapped addresses are acceptable. */
	var AI_V4MAPPED = 8;
	/** Return IPv4 mapped and IPv6 addresses. */
	var AI_ALL = 16;
	/** Use configuration of this host to choose returned address type. */
	var AI_ADDRCONFIG = 32;
	/** Don't use name resolution. */
	var AI_NUMERICSERV = 64;
}

typedef AddrInfoOptions = {
	var ?flags:AddrInfoFlags;
	var ?family:AddressFamily;
	var ?sockType:SocketType;
	var ?protocol:Int;
}

typedef AddrInfo = {
	var family:AddressFamily;
	var sockType:SocketType;
	var protocol:Int;
	var addr:SockAddr;
	var ?canonName:String;
}

enum abstract NameInfoFlags(Int) from Int to Int {
	/** Don't try to look up hostname. */
	var NI_NUMERICHOST = 1;
	/** Don't convert port number to name. */
	var NI_NUMERICSERV = 2;
	/** Only return nodename portion. */
	var NI_NOFQDN = 4;
	/** Don't return numeric addresses. */
	var NI_NAMEREQD = 8;
	/** Look up UDP service rather than TCP. */
	var NI_DGRAM = 16;
}

private class AddrData extends RequestData {
	public final callback:(e:UVError, ai:RawAddrInfo)->Void;

	public function new(callback) {
		this.callback = callback;
	}
}

private class NameData extends RequestData {
	public final callback:(e:UVError, hostname:Bytes, service:Bytes)->Void;

	public function new(callback) {
		this.callback = callback;
	}
}

@:forward abstract AddrInfoRequest(Request) to Request {}

@:forward abstract NameInfoRequest(Request) to Request {}

/**
	DNS queries.

	@see http://docs.libuv.org/en/v1.x/dns.html
**/
class Dns {
	/**
		Retrieves addresses.

		Either `name` or `service` may be `null` but not both.
	**/
	static public function getAddrInfo(loop:Loop, name:Null<String>, service:Null<String>, hints:Null<AddrInfoOptions>,
		callback:(e:UVError, infos:Array<AddrInfo>)->Void):AddrInfoRequest {

		var node = name == null ? null : @:privateAccess name.toUtf8();
		var service = service == null ? null : @:privateAccess service.toUtf8();
		var aiHints:RawAddrInfo = null;

		var req = UV.alloc_getaddrinfo();
		req.setData(new AddrData((e, ai) -> {
			// TODO: These are freed somewhere else. Figure out why (especially `req`)
			// UV.free(node);
			// UV.free(service);
			// aiHints.freeaddrinfo();
			req.setData(null);
			// UV.free(req);

			var infos = null;
			if(ai != null) {
				infos = [];
				while(ai != null) {
					var entry:AddrInfo = {
						family:ai.addrinfo_ai_family(),
						sockType:ai.addrinfo_ai_socktype(),
						protocol:ai.addrinfo_ai_protocol(),
						addr:ai.addrinfo_ai_addr()
					}
					switch ai.addrinfo_ai_canonname() {
						case null:
						case cn: entry.canonName = @:privateAccess String.fromUTF8(cn);
					}
					infos.push(entry);
					ai = ai.addrinfo_ai_next();
				}
				ai.freeaddrinfo();
			}
			callback(e, infos);
		}));

		if(hints != null) {
			aiHints = UV.alloc_addrinfo(hints.flags, hints.family, hints.sockType, hints.protocol);
		}
		loop.getaddrinfo_with_cb(req, node, service, aiHints);

		return req;
	}

	/**
		Retrieves host names.
	**/
	static public function getNameInfo(loop:Loop, addr:SockAddr, flags:NameInfoFlags,
		callback:(e:UVError, name:String, service:String)->Void):NameInfoRequest {

		var req = UV.alloc_getnameinfo();
		req.setData(new NameData((e, hostname, service) -> {
			req.setData(null);
			// UV.free(req);
			callback(
				e,
				hostname == null ? null : @:privateAccess String.fromUTF8(hostname),
				service == null ? null : @:privateAccess String.fromUTF8(service)
			);
			// UV.free(hostname);
			// UV.free(service);
		}));
		loop.getnameinfo_with_cb(req, addr, flags.nameinfo_flags_to_native());

		return req;
	}
}
