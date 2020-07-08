package asys.native.net;

import haxe.exceptions.NotImplementedException;

/**
	Methods related to Domain Name System.
**/
class Dns {
	/**
		Lookup the given `host` name.
	**/
	static public function resolve(host:String, callback:Callback<Array<Ip>>) {
		throw new NotImplementedException();
	}

	/**
		Find host names associated with the given IP address.
	**/
	static public function reverse(ip:Ip, callback:Callback<Array<String>>) {
		throw new NotImplementedException();
	}
}