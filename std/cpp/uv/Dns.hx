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

import cpp.uv.SockAddr;

using cpp.uv.UV;

enum abstract AddrInfoFlag(Int) {
	var AIF_PASSIVE;
	var AIF_CANONNAME;
	var AIF_NUMERICHOST;
	var AIF_V4MAPPED;
	var AIF_ALL;
	var AIF_ADDRCONFIG;
	var AIF_NUMERICSERV;
}

typedef AddrInfoOptions = {
	var ?flags:Array<AddrInfoFlag>;
	var ?family:AddressFamily;
	var ?sockType:SocketType;
	var ?protocol:Int;
}

@:structInit
class AddrInfo {
	public var family:AddressFamily;
	public var sockType:SocketType;
	public var protocol:Int;
	public var addr:SockAddr;
	public var canonName:Null<String>;
}

enum abstract NameInfoFlag(Int) {
	var NIF_NUMERICHOST;
	var NIF_NUMERICSERV;
	var NIF_NOFQDN;
	var NIF_NAMEREQD;
	var NIF_DGRAM;
}

@:allow(cpp.uv)
@:headerCode('#include "uv.h"')
class AddrInfoRequest extends Request {
	var callback:(e:UVError, infos:Array<AddrInfo>)->Void;
	var aiHints:RawPointer<Addrinfo>;
	var uvAddrInfo(get,never):RawPointer<UvGetaddrinfoT>;

	inline function get_uvAddrInfo():RawPointer<UvGetaddrinfoT>
		return cast uv;

	override function setupUvData() {
		uv = cast UvGetaddrinfoT.create();
		super.setupUvData();
	}

	override function finalize() {
		super.finalize();
		UV.freeaddrinfo(aiHints);
	}
}

@:allow(cpp.uv)
@:headerCode('#include "uv.h"')
class NameInfoRequest extends Request {
	var callback:(e:UVError, name:String, service:String)->Void;
	var uvNameInfo(get,never):RawPointer<UvGetnameinfoT>;

	inline function get_uvNameInfo():RawPointer<UvGetnameinfoT>
		return cast uv;

	override function setupUvData() {
		uv = cast UvGetnameinfoT.create();
		super.setupUvData();
	}
}

/**
	DNS utility functions

	@see http://docs.libuv.org/en/v1.x/dns.html
**/
@:headerCode('#include "uv.h"')
class Dns {

	/**
		Retrieves addresses.

		Either `name` or `service` may be `null` but not both.
	**/
	static public function getAddrInfo(loop:Loop, name:Null<String>, service:Null<String>, hints:Null<AddrInfoOptions>,
		callback:(e:UVError, infos:Array<AddrInfo>)->Void):AddrInfoRequest {

		var req = new AddrInfoRequest();
		var node = name == null ? null : ConstCharStar.fromString(name);
		var service = service == null ? null : ConstCharStar.fromString(service);
		if(hints != null) {
			req.aiHints = Addrinfo.create();
			var aiHints = Pointer.fromRaw(req.aiHints);
			if(hints.flags != null) {
				aiHints.value.ai_flags = 0;
				for(flag in hints.flags)
					aiHints.value.ai_flags |= switch flag {
						case AIF_PASSIVE: AI_PASSIVE;
						case AIF_CANONNAME: AI_CANONNAME;
						case AIF_NUMERICHOST: AI_NUMERICHOST;
						case AIF_V4MAPPED: AI_V4MAPPED;
						case AIF_ALL: AI_ALL;
						case AIF_ADDRCONFIG: AI_ADDRCONFIG;
						case AIF_NUMERICSERV: AI_NUMERICSERV;
					}
			}
			if(hints.family != null)
				aiHints.value.ai_family = SockAddr.addressFamilyToAf(hints.family);
			if(hints.sockType != null)
				aiHints.value.ai_socktype = SockAddr.socketTypeToNative(hints.sockType);
			if(hints.protocol != null)
				aiHints.value.ai_protocol = hints.protocol;
		}
		UV.getaddrinfo(loop.uvLoop, req.uvAddrInfo, Callable.fromStaticFunction(uvGetaddrinfoCb), node, service, req.aiHints).resolve();
		req.callback = callback;
		return req;
	}

	static function uvGetaddrinfoCb(uvAddrInfo:RawPointer<UvGetaddrinfoT>, status:Int, res:RawPointer<Addrinfo>) {
		var req:AddrInfoRequest = cast Request.get(cast uvAddrInfo);
		var infos = null;
		if(res != null) {
			infos = [];
			var ptr = Pointer.fromRaw(res);
			while(ptr != null) {
				var entry:AddrInfo = {
					family: SockAddr.afToAddressFamily(ptr.value.ai_family),
					sockType: SockAddr.nativeToSocketType(ptr.value.ai_socktype),
					protocol: ptr.value.ai_protocol,
					addr: new SockAddr(),
					canonName: ptr.value.ai_canonname == null ? null : ptr.value.ai_canonname.charStarToString()
				}
				untyped __cpp__('memcpy({0}, {1}, {2})', entry.addr.storage, ptr.value.ai_addr, ptr.value.ai_addrlen);
				infos.push(entry);
				ptr = Pointer.fromRaw(ptr.value.ai_next);
			}
			UV.freeaddrinfo(res);
		}
		req.callback(status.explain(), infos);
	}

	/**
		Retrieves host names.
	**/
	static public function getNameInfo(loop:Loop, addr:SockAddr, flags:Null<Array<NameInfoFlag>>,
		callback:(e:UVError, name:String, service:String)->Void):NameInfoRequest {

		var req = new NameInfoRequest();
		var iFlags = 0;
		if(flags != null)
			for(flag in flags)
				iFlags |= switch flag {
					case NIF_NUMERICHOST: NI_NUMERICHOST;
					case NIF_NUMERICSERV: NI_NUMERICSERV;
					case NIF_NOFQDN: NI_NOFQDN;
					case NIF_NAMEREQD: NI_NAMEREQD;
					case NIF_DGRAM: NI_DGRAM;
				}
		UV.getnameinfo(loop.uvLoop, req.uvNameInfo, Callable.fromStaticFunction(uvGetnameinfoCb), cast addr.storage, iFlags).resolve();
		req.callback = callback;
		return req;
	}

	static function uvGetnameinfoCb(uvNameInfo:RawPointer<UvGetnameinfoT>, status:Int, hostname:ConstCharStar, service:ConstCharStar) {
		var req:NameInfoRequest = cast Request.get(cast uvNameInfo);
		req.callback(
			status.explain(),
			hostname == null ? null : hostname.toString(),
			service == null ? null : service.toString()
		);
	}
}