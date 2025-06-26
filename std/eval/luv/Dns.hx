package eval.luv;

@:forward
abstract AddrInfoRequest(Request) to Request {}

typedef AddrInfo = {
	var family:SockAddr.AddressFamily;
	var sockType:SockAddr.SocketType;
	var protocol:Int;
	var addr:SockAddr;
	var ?canonName:String;
}

typedef AddrInfoOptions = {
	var ?request:AddrInfoRequest;
	var ?family:SockAddr.AddressFamily;
	var ?sockType:SockAddr.SocketType;
	var ?protocol:Int;
	var ?flags:Array<AddrInfoFlag>;
}

enum abstract AddrInfoFlag(Int) {
	var PASSIVE = 0;
	var CANONNAME = 1;
	var NUMERICHOST = 2;
	var NUMERICSERV = 3;
	var V4MAPPED = 4;
	var ALL = 5;
	var ADDRCONFIG = 6;
}

@:forward
abstract NameInfoRequest(Request) to Request {}

enum abstract NameInfoFlag(Int) {
	var NAMEREQD = 0;
	var DGRAM = 1;
	var NOFQDN = 2;
	var NUMERICHOST = 3;
	var NUMERICSERV = 4;
}

typedef NameInfoOptions = {
	var ?request:NameInfoRequest;
	var ?flags:Array<NameInfoFlag>;
}

/**
	DNS queries.

	@see https://aantron.github.io/luv/luv/Luv/Dns
**/
extern class Dns {

	static function createAddrRequest():AddrInfoRequest;

	static function createNameRequest():NameInfoRequest;

	/**
		Retrieves addresses.
		Either `node` or `service` may be `null` but not both.
	**/
	static function getAddrInfo(loop:Loop, node:Null<String>, service:Null<String>, ?options:AddrInfoOptions, callback:(result:Result<Array<AddrInfo>>)->Void):Void;

	/**
		Retrieves host names.
	**/
	static function getNameInfo(loop:Loop, addr:SockAddr, ?options:NameInfoOptions, callback:(result:Result<{hostName:String, service:String}>)->Void):Void;
}