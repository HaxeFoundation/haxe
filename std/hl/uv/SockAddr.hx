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
abstract SockAddr(hl.Abstract<"uv_sockaddr">) {
	/** Extracts the port in a network address. */
	public var port(get,never):Null<Int>;
	@:hlNative("uv", "get_port") function get_port():Int return null;

	/**
		Converts a string and port number to an IPv4 struct sockaddr.

		@see http://docs.libuv.org/en/v1.x/misc.html#c.uv_ip4_addr
	**/
	@:hlNative("uv", "ip4_addr_wrap")
	static public function ipv4(host:String, port:Int):Result<SockAddr>;

	/**
		Converts a string and port number to an IPv6 struct sockaddr.

		@see http://docs.libuv.org/en/v1.x/misc.html#c.uv_ip6_addr
	**/
	@:hlNative("uv", "ip6_addr_wrap")
	static public function ipv6(host:String, port:Int):Result<SockAddr>;

	/**
		Converts a network address to a string.

		@see http://docs.libuv.org/en/v1.x/misc.html#c.uv_ip4_name
		@see http://docs.libuv.org/en/v1.x/misc.html#c.uv_ip6_name
	**/
	@:hlNative("uv", "up_name_wrap")
	public function name():String
		return null;

	/**
		Converts a network address to a string.
	**/
	public inline function toString():String
		return name();
}