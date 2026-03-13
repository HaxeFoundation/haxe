package sys.net;

@:coreApi
@:allow(sys.net.Socket)
class Host {
	#if (haxe_ver >= 3.3) public #end var host(default, null):String;

	public var ip(default, null):Int;

	var ipStr:String;

	public function new(name:String):Void {
		var ret = sys.NodeSync.callMany(function(callb) js.node.Dns.lookup(name, {family: 4}, callb));
		if (ret[0] != null)
			throw ret[0]; // error
		this.host = name;
		ipStr = ret[1];
		var parts = [for (p in ipStr.split(".")) Std.parseInt(p)];
		ip = (parts[0] << 24) | (parts[1] << 16) | (parts[2] << 8) | parts[3];
	}

	public function toString():String {
		return ipStr;
	}

	public function reverse():String {
		throw "Not implemented";
		return null;
	}

	public static function localhost():String {
		throw "Not implemented";
		return null;
	}
}
