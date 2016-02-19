package sys.ssl;

private typedef SocketHandle = Dynamic;
private typedef CTX = Dynamic;
private typedef SSL = Dynamic;

private class SocketInput extends haxe.io.Input {
	@:allow(sys.ssl.Socket) private var __s : SocketHandle;
	@:allow(sys.ssl.Socket) private var ssl : SSL;

	public function new( s : SocketHandle, ?ssl : SSL ) {
		this.__s = s;
		this.ssl = ssl;
	}

	public override function readByte() {
		return try {
			socket_recv_char( ssl );
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw haxe.io.Error.Blocked;
			else if( __s == null )
				throw haxe.io.Error.Custom(e);
			else
				throw new haxe.io.Eof();
		}
	}

	public override function readBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) : Int {
		var r : Int;
		if( ssl == null || __s == null )
			throw "Invalid handle";
		try {
			r = socket_recv(  ssl, buf.getData(), pos, len );
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw haxe.io.Error.Blocked;
			else
				throw haxe.io.Error.Custom(e);
		}
		if( r == 0 )
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if( __s != null ) socket_close( __s );
	}

	private static var socket_recv = neko.Lib.load( "ssl", "ssl_recv", 4 );
	private static var socket_recv_char = neko.Lib.load( "ssl", "ssl_recv_char", 1 );
	private static var socket_close = neko.Lib.load( "std", "socket_close", 1 );

}

private class SocketOutput extends haxe.io.Output {
	@:allow(sys.ssl.Socket) private var __s : SocketHandle;
	@:allow(sys.ssl.Socket) private var ssl : SSL;

	public function new( s : SocketHandle, ?ssl : SSL ) {
		this.__s = s;
		this.ssl = ssl;
	}

	public override function writeByte( c : Int ) {
		if( __s == null )
			throw "Invalid handle";
		try {
			socket_send_char( ssl, c);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw haxe.io.Error.Blocked;
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function writeBytes( buf : haxe.io.Bytes, pos : Int, len : Int) : Int {
		return try {
			socket_send( ssl, buf.getData(), pos, len);
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw haxe.io.Error.Blocked;
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function close() {
		super.close();
		if( __s != null ) socket_close(__s);
	}

	private static var socket_close = neko.Lib.load( "std", "socket_close", 1 );
	private static var socket_send_char = neko.Lib.load( "ssl", "ssl_send_char", 2 );
	private static var socket_send = neko.Lib.load( "ssl", "ssl_send", 4 );
}

@:coreApi
class Socket implements sys.net.ISocket {
	
	public static var DEFAULT_VERIFY_CERT : Null<Bool> = true;

	public static var DEFAULT_CA : Null<Certificate>;
	
	public var input(default,null) : haxe.io.Input;
	public var output(default,null) : haxe.io.Output;
	public var custom : Dynamic;

	private var __s : SocketHandle;
	private var ctx : CTX;
	private var ssl : SSL;
	
	public var verifyCert : Null<Bool>;
	private var caCert : Null<Certificate>;
	private var hostname : String;

	private var ownCert : Null<Certificate>;
	private var ownKey : Null<Key>;
	private var altSNIContexts : Null<Array<{match: String->Bool, key: Key, cert: Certificate}>>;
	private var sniCallback : Dynamic;

	public function new() {
		__s = socket_new( false );
		input = new SocketInput( __s );
		output = new SocketOutput( __s );
		if( DEFAULT_VERIFY_CERT && DEFAULT_CA == null ){
			try {
				DEFAULT_CA = Certificate.loadDefaults();
			}catch( e : Dynamic ){}
		}
		verifyCert = DEFAULT_VERIFY_CERT;
		caCert = DEFAULT_CA;
	}

	public function connect(host : sys.net.Host, port : Int) : Void {
		try {
			ctx = buildSSLContext( false );
			ssl = ssl_new( ctx );
			ssl_set_socket( ssl, __s );
			var input : SocketInput = cast input;
			var output : SocketOutput = cast output;
			input.ssl = ssl;
			output.ssl = ssl;
			if( hostname == null )
				hostname = host.host;
			if( hostname != null )
				ssl_set_hostname( ssl, untyped hostname.__s );
			socket_connect( __s, host.ip, port );
			handshake();
		} catch( s : String ) {
			if( s == "std@socket_connect" )
				throw "Failed to connect on "+host.host+":"+port;
			else
				neko.Lib.rethrow(s);
		} catch( e : Dynamic ) {
			neko.Lib.rethrow(e);
		}
	}

	public function handshake() : Void {
		try {
			ssl_handshake( ssl );
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw haxe.io.Error.Blocked;
			else
				neko.Lib.rethrow( e );
		}
	}

	public function setCA( cert : Certificate ) : Void {
		caCert = cert;
	}

	public function setHostname( name : String ) : Void {
		hostname = name;
	}

	public function setCertificate( cert : Certificate, key : Key ) : Void {
		ownCert = cert;
		ownKey = key;
	}

	public function read() : String {
		var b = ssl_read( ssl );
		if( b == null )
			return "";
		return new String(cast b);
	}

	public function write( content : String ) : Void {
		ssl_write( ssl, untyped content.__s );
	}

	public function close() : Void {
		if( ssl != null ) ssl_close( ssl );
		if( ctx != null ) conf_close( ctx );
		if( altSNIContexts != null )
			sniCallback = null;
		socket_close( __s );
		var input : SocketInput = cast input;
		var output : SocketOutput = cast output;
		@:privateAccess input.__s = output.__s = null;
		@:privateAccess input.ssl = output.ssl = null;
		input.close();
		output.close();
	}

	public function addSNICertificate( cbServernameMatch : String->Bool, cert : Certificate, key : Key ) : Void {
		if( altSNIContexts == null )
			altSNIContexts = [];
		altSNIContexts.push( {match: cbServernameMatch, cert: cert, key: key} );
	}

	public function bind( host : sys.net.Host, port : Int ) : Void {
		ctx = buildSSLContext( true );

		socket_bind( __s, host.ip, port );
	}

	public function listen( connections : Int ) : Void {
		socket_listen( __s, connections );
	}

	public function accept() : Socket {
		var c = socket_accept( __s );
		var ssl = ssl_new( ctx );
		ssl_set_socket( ssl, c );

		var s = Type.createEmptyInstance( sys.ssl.Socket );
		s.__s = c;
		s.ssl = ssl;
		s.input = new SocketInput(c, ssl);
		s.output = new SocketOutput(c, ssl);

		return s;
	}

	public function peer() : { host : sys.net.Host, port : Int } {
		var a : Dynamic = socket_peer(__s);
		var h = new sys.net.Host("127.0.0.1");
		untyped h.ip = a[0];
		return { host : h, port : a[1] };
	}

	public function peerCertificate() : sys.ssl.Certificate {
		var x = ssl_get_peer_certificate( ssl );
		return x==null ? null : new sys.ssl.Certificate( x );
	}

	public function shutdown( read : Bool, write : Bool ) : Void {
		socket_shutdown( __s, read, write );
	}

	public function host() : { host : sys.net.Host, port : Int } {
		var a : Dynamic = socket_host( __s );
		var h = new sys.net.Host( "127.0.0.1" );
		untyped h.ip = a[0];
		return { host : h, port : a[1] };
	}

	public function setTimeout( timeout : Float ) : Void {
		socket_set_timeout( __s, timeout );
	}

	public function waitForRead() : Void {
		sys.net.Socket.select([__s],null,null,null);
	}

	public function setBlocking( b : Bool ) : Void {
		socket_set_blocking(__s,b);
	}

	public function setFastSend( b : Bool ) : Void {
		socket_set_fast_send(__s,b);
	}

	private function buildSSLContext( server : Bool ) : CTX {
		var ctx : CTX = conf_new( server );

		if( ownCert != null && ownKey != null )
			conf_set_cert( ctx, @:privateAccess ownCert.__x, @:privateAccess ownKey.__k );

		if ( altSNIContexts != null ) {
			sniCallback = function(servername) {
				var servername = new String(cast servername);
				for( c in altSNIContexts ){
					if( c.match(servername) )
						return @:privateAccess {key: c.key.__k, cert: c.cert.__x};
				}
				if( ownKey != null && ownCert != null )
					return @:privateAccess { key: ownKey.__k, cert: ownCert.__x };
				return null;
			}
			conf_set_servername_callback( ctx, sniCallback );
		}

		if ( caCert != null ) 
			conf_set_ca( ctx, caCert == null ? null : @:privateAccess caCert.__x  );
		conf_set_verify( ctx, verifyCert );
		
		return ctx;
	}
	
	private static var ssl_new = neko.Lib.load( "ssl", "ssl_new", 1 );
	private static var ssl_close = neko.Lib.load( "ssl", "ssl_close", 1 );
	private static var ssl_handshake = neko.Lib.load( "ssl", "ssl_handshake", 1 );
	private static var ssl_set_socket = neko.Lib.load( "ssl", "ssl_set_socket", 2 );
	private static var ssl_set_hostname = neko.Lib.load( "ssl", "ssl_set_hostname", 2 );
	private static var ssl_get_peer_certificate = neko.Lib.load( "ssl", "ssl_get_peer_certificate", 1 );

	private static var ssl_read = neko.Lib.load( "ssl", "ssl_read", 1 );
	private static var ssl_write = neko.Lib.load( "ssl", "ssl_write", 2 );

	private static var conf_new = neko.Lib.load( "ssl", "conf_new", 1 );
	private static var conf_close = neko.Lib.load( "ssl", "conf_close", 1 );
	private static var conf_set_ca = neko.Lib.load( "ssl", "conf_set_ca", 2 );
	private static var conf_set_verify = neko.Lib.load( "ssl", "conf_set_verify", 2 );
	private static var conf_set_cert = neko.Lib.load( "ssl", "conf_set_cert", 3 );
	private static var conf_set_servername_callback = neko.Lib.load( "ssl", "conf_set_servername_callback", 2 );

	private static var socket_new = neko.Lib.load("std","socket_new",1);
	private static var socket_close = neko.Lib.load("std","socket_close",1);
	private static var socket_connect = neko.Lib.load("std","socket_connect",3);
	private static var socket_listen = neko.Lib.load("std","socket_listen",2);
	private static var socket_bind = neko.Lib.load("std","socket_bind",3);
	private static var socket_accept = neko.Lib.load("std","socket_accept",1);
	private static var socket_peer = neko.Lib.load("std","socket_peer",1);
	private static var socket_host = neko.Lib.load("std","socket_host",1);
	private static var socket_set_timeout = neko.Lib.load("std","socket_set_timeout",2);
	private static var socket_shutdown = neko.Lib.load("std","socket_shutdown",3);
	private static var socket_set_blocking = neko.Lib.load("std","socket_set_blocking",2);
	private static var socket_set_fast_send = neko.Lib.loadLazy("std","socket_set_fast_send",2);

}
