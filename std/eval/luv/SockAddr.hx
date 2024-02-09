package eval.luv;

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

/**
	Binds `struct sockaddr`.

	@see https://aantron.github.io/luv/luv/Luv/Sockaddr
**/
@:coreType abstract SockAddr {
	/** Extracts the port in a network address. */
	public var port(get,never):Null<Int>;
	function get_port():Null<Int>;

	/**
		Converts a string and port number to an IPv4 struct sockaddr.
	**/
	static public function ipv4(host:String, port:Int):Result<SockAddr>;

	/**
		Converts a string and port number to an IPv6 struct sockaddr.
	**/
	static public function ipv6(host:String, port:Int):Result<SockAddr>;

	/**
		Converts a network address to a string.
	**/
	public function toString():String;
}