package eval.luv;

typedef InterfaceAddress = {
	var name:String;
	var isInternal:Bool;
	var physical:NativeString;
	var address:SockAddr;
	var netmask:SockAddr;
}

/**
	Network interfaces and hostname.

	@see https://aantron.github.io/luv/luv/Luv/Network
**/
extern class Network {
	/**
		Lists network interface addresses.
	**/
	static function interfaceAddresses():Result<Array<InterfaceAddress>>;

	/**
		Retrieves a network interface name.
	**/
	static function ifIndexToName(index:Int):Result<String>;

	/**
		Retrieves a network interface identifier suitable for use in an IPv6 scoped address.
	**/
	static function ifIndexToIid(index:Int):Result<String>;

	/**
		Evaluates to the system's hostname.
	**/
	static function getHostName():Result<String>;
}