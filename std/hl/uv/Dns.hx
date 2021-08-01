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

typedef AddrInfoOptions = {
	var ?flags:{
		var ?passive:Bool;
		var ?canonName:Bool;
		var ?numericHost:Bool;
		var ?numericServ:Bool;
		var ?v4Mapped:Bool;
		var ?all:Bool;
		var ?addrConfig:Bool;
	};
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
// @:forward
// abstract AddrInfo(AddrInfoData) from AddrInfoData {
// 	public var canonName(get,never):String;
// }

typedef NameInfoFlags = {
	var ?nameReqd:Bool;
	var ?dgram:Bool;
	var ?noFqdn:Bool;
	var ?numericHost:Bool;
	var ?numericServ:Bool;
}

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
		callback:(e:UVError, infos:Array<AddrInfo>)->Void):Void {
		function onAddrInfo(e:UVError, infos:NativeArray<Dynamic>) {
			callback(e, [for(i in 0...infos.length) {
				var entry:Dynamic = infos[i];
				if(entry.canonName != null) {
					entry.canonName = @:privateAccess String.fromUTF8(entry.canonName);
				}
				entry.addr = @:privateAccess SockAddr.castPtr(entry.addr);
				(entry:AddrInfo);
			}]);
		}
		if(hints == null) {
			getAddrInfoWrap(loop, name, service, null, null, null, null, onAddrInfo);
		} else {
			getAddrInfoWrap(loop, name, service, hints.flags, hints.family, hints.sockType, hints.protocol, onAddrInfo);
		}
	}

	@:hlNative("uv", "getaddrinfo_wrap")
	static function getAddrInfoWrap(
		loop:Loop,
		name:Null<String>,
		service:Null<String>,
		flags:Null<Dynamic>,
		family:Null<AddressFamily>,
		sockType:Null<SocketType>,
		protocol:Null<Int>,
		callback:(e:UVError, infos:NativeArray<AddrInfo>)->Void
	):Void {}

	/**
		Retrieves host names.
	**/
	static public function getNameInfo(loop:Loop, addr:SockAddr, flags:Null<NameInfoFlags>,
		callback:(e:UVError, name:String, service:String)->Void):Void {
		function onNameInfo(e:UVError, name:Bytes, service:Bytes) @:privateAccess {
			var name = name == null ? null : String.fromUTF8(name);
			var service = service == null ? null : String.fromUTF8(service);
			callback(e, name, service);
		}
		if(flags == null) {
			getNameInfoWrap(loop, addr, null, null, null, null, null, onNameInfo);
		} else {
			getNameInfoWrap(loop, addr, flags.nameReqd, flags.dgram, flags.noFqdn, flags.numericHost, flags.numericServ, onNameInfo);
		}
	}

	@:hlNative("uv", "getnameinfo_wrap")
	static public function getNameInfoWrap(
		loop:Loop,
		addr:SockAddr,
		nameReqd:Null<Bool>,
		dgram:Null<Bool>,
		noFqdn:Null<Bool>,
		numericHost:Null<Bool>,
		numericServ:Null<Bool>,
		callback:(e:UVError, name:Null<Bytes>, service:Null<Bytes>)->Void
	):Void {}

}
