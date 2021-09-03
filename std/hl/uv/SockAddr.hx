package hl.uv;

using hl.uv.UV;

/**
	Network address families.

	Does not match native values!
**/
enum abstract AddressFamily(Int) from Int {
	var UNSPEC = -1;
	var INET = -2;
	var INET6 = -3;
}

/**
	Socket types.

	TODO:
	Use native values
**/
enum abstract SocketType(Int) from Int {
	var STREAM = -1;
	var DGRAM = -2;
	var RAW = -3;
}

/**
	Represents ip4 or ip6 socket address.
**/
@:using(hl.uv.SockAddr.SockAddrTools)
enum SockAddr {
	Ip4Addr(ip:String, ?port:Int);
	Ip6Addr(ip:String, ?port:Int);
}

class SockAddrTools {
	/** Extracts the port in a network address. */
	static public function port(addr:Null<SockAddr>):Null<Int> {
		return switch addr {
			case null: null;
			case Ip4Addr(_, port): port;
			case Ip6Addr(_, port): port;
		}
	}

	@:allow(hl.uv)
	static function ofSockaddrStorageStar(addr:Null<CSockaddrStorageStar>):Null<SockAddr> {
		if(addr == null)
			return null;
		var buf = new Bytes(256);
		var size = I64.ofInt(256);
		return switch addr.sockaddr_storage_ss_family().address_family_of_af() {
			case INET:
				UV.ip4_name(addr.sockaddr_in_of_storage(), buf, size);
				Ip4Addr(buf.fromUTF8(), addr.sockaddr_in_of_storage().sockaddr_in_sin_port());
			case INET6:
				UV.ip6_name(addr.sockaddr_in6_of_storage(), buf, size);
				Ip6Addr(buf.fromUTF8(), addr.sockaddr_in6_of_storage().sockaddr_in6_sin6_port());
			case _:
				null;
		}
	}

	@:allow(hl.uv)
	static function toSockaddrStorageStar(addr:Null<SockAddr>):Null<CSockaddrStorageStar> {
		if(addr == null)
			return null;
		var cAddr = UV.alloc_sockaddr_storage();
		var result = switch addr {
			case Ip4Addr(ip, port): UV.ip4_addr(ip.toUTF8(), port, cAddr.sockaddr_in_of_storage());
			case Ip6Addr(ip, port): UV.ip6_addr(ip.toUTF8(), port, cAddr.sockaddr_in6_of_storage());
		}
		if(result < 0) {
			cAddr.free_sockaddr_storage();
			result.throwErr();
		}
		return cAddr;
	}

	/**
		Converts a network address to a string.
	**/
	static public function name(addr:Null<SockAddr>):Null<String> {
		return switch addr {
			case null: null;
			case Ip4Addr(ip, _): ip;
			case Ip6Addr(ip, _): ip;
		}
	}

	/**
		Converts a network address to a string.
	**/
	static public function toString(addr:Null<SockAddr>):Null<String> {
		return switch addr {
			case null: null;
			case Ip4Addr(ip, port): ip + (port == null ? '' : '$port');
			case Ip6Addr(ip, port): ip + (port == null ? '' : '$port');
		}
	}
}