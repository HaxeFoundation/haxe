package hl.uv;

/**
	Network address families.

	TODO:
	Use native values
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
	Binds `struct sockaddr` (man 2 socket).
**/
abstract SockAddr(CSockaddrStar) to CSockaddrStar {
	/** Extracts the port in a network address. */
	public var port(get,never):Null<Int>;
	@:hlNative("uv", "sockaddr_get_port") function get_port():Null<Int> return null;

	@:hlNative("uv", "sockaddr_cast_ptr")
	static function castPtr(pointer:Dynamic):SockAddr
		return null;

	/**
		Converts a string and port number to an IPv4 struct sockaddr.

		@see http://docs.libuv.org/en/v1.x/misc.html#c.uv_ip4_addr
	**/
	@:hlNative("uv", "ip4_addr_wrap")
	static public function ipv4(ip:String, port:Int):SockAddr
		return null;

	/**
		Converts a string and port number to an IPv6 struct sockaddr.

		@see http://docs.libuv.org/en/v1.x/misc.html#c.uv_ip6_addr
	**/
	@:hlNative("uv", "ip6_addr_wrap")
	static public function ipv6(ip:String, port:Int):SockAddr
		return null;

	/**
		Converts a network address to a string.

		@see http://docs.libuv.org/en/v1.x/misc.html#c.uv_ip4_name
		@see http://docs.libuv.org/en/v1.x/misc.html#c.uv_ip6_name
	**/
	public function name():Null<String>
		return switch name_wrap() {
			case null: null;
			case b: @:privateAccess String.fromUTF8(b);
		}
	//TODO: return `String` instead of `Bytes`
	@:hlNative("uv", "ip_name_wrap") function name_wrap():Null<Bytes>
		return null;

	/**
		Converts a network address to a string.
	**/
	public function toString():String
		return switch port {
			case null: '${name()}';
			case p: '${name()}:$p';
		}
}