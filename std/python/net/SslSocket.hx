package python.net;

import python.lib.Ssl;
import python.lib.ssl.Purpose;
import python.lib.net.Socket as PSocket;
import sys.net.Host;

class SslSocket extends sys.net.Socket {

	var hostName:String;

	override function __initSocket ():Void {
		var context = Ssl.create_default_context(Purpose.SERVER_AUTH);
		context.options |= Ssl.OP_NO_TLSv1; // python 3.4 | Ssl.OP_NO_TLSv1_1;
		__s = new PSocket();
		__s = context.wrap_socket(__s,
			false,
			true,
			true,
			this.hostName
		);
	}

	public override function connect( host : Host, port : Int ) : Void {
		 this.hostName = host.host;
		 super.connect(host, port);
	}
	public override function bind( host : Host, port : Int ) : Void {
		throw "not implemented";
	}
}