package asys.net;

import haxe.async.*;

/**
	Asynchronous Domain Name System (DNS) methods.
**/
extern class Dns {
	/**
		Looks up the given `hostname`. `callback` will be called once the operation
		completes. In case of success, the data given to callback is an array of
		`asys.net.Address` instances representing all the IP addresses found
		associated with the hostname.

		- `lookupOptions.family` - if not `null`, only addresses of the given IP
			family will be returned.
	**/
	static function lookup(hostname:String, ?lookupOptions:DnsLookupOptions, callback:Callback<Array<Address>>):Void;

	/**
		Looks up a reverse DNS entry for the given `ip`.
	**/
	static function reverse(ip:Address, callback:Callback<Array<String>>):Void;
}
