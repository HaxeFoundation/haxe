package asyncio.net;

import haxe.errors.NotImplemented;
import haxe.Callback;

/**
	Methods related to Domain Name System.
**/
class Dns {
	/**
		Lookup the given `host` name.
	**/
	static public function resolve(host:String, callback:Callback<Null<Array<Ip>>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Find host names associated with the given IP address.
	**/
	static public function reverse(ip:Ip, callback:Callback<Null<Array<String>>>) {
		callback.fail(new NotImplemented());
	}
}