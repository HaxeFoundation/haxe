package asys.net;

import haxe.async.Callback;

using asys.net.AddressTools;

class Dns {
	static extern function lookup_native(hostname:String, ?lookupOptions:DnsLookupOptions, callback:Callback<Array<Address>>);

	public static function lookup(hostname:String, ?lookupOptions:DnsLookupOptions, callback:Callback<Array<Address>>):Void {
		lookup_native(hostname, lookupOptions, function (err, res:Array<Address>):Void {
			if (err != null)
				return callback(err, null);
			var lastRes:Address = null;
			callback(null, [ for (entry in res) {
				// TODO: report more information rather than suppress duplicates?
				if (lastRes != null && lastRes.equals(entry))
					continue;
				lastRes = entry;
			} ]);
		});
	}

	public static extern function reverse(ip:Address, callback:Callback<Array<String>>):Void;
}
