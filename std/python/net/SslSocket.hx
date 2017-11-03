package python.net;

import python.lib.Ssl;
import python.lib.ssl.Purpose;
import python.lib.net.Socket as PSocket;
import sys.net.Host;

class SslSocket extends sys.net.Socket {

	var hostName:String;

	override function __initSocket ():Void {

		#if (python_version >= 3.4)
		var context = Ssl.create_default_context(Purpose.SERVER_AUTH);
		#else
		// hopefully these options are good enough
		var context = new python.lib.ssl.SSLContext(Ssl.PROTOCOL_SSLv23);
		context.verify_mode = Ssl.CERT_REQUIRED;
		context.set_default_verify_paths();
		context.options |= Ssl.OP_NO_SSLv2;
		context.options |= Ssl.OP_NO_SSLv3;
		context.options |= Ssl.OP_NO_COMPRESSION;
		#end
		context.options |= Ssl.OP_NO_TLSv1 #if (python_version >= 3.4) | Ssl.OP_NO_TLSv1_1 #end; // python 3.4 | Ssl.OP_NO_TLSv1_1;
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