package sys.ssl;
import cpp.NativeSocket;
import cpp.NativeSsl;

private typedef SocketHandle = Dynamic;
private typedef CONF = Dynamic;
private typedef SSL = Dynamic;

private class SocketInput extends haxe.io.Input {
	@:allow(sys.ssl.Socket) private var __s : Socket;

	public function new( s : Socket ) {
		this.__s = s;
	}

	public override function readByte() {
		return try {
			__s.handshake();
			NativeSsl.ssl_recv_char( @:privateAccess __s.ssl );
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
		if( __s == null )
			throw "Invalid handle";
		try {
			__s.handshake();
			r = NativeSsl.ssl_recv( @:privateAccess __s.ssl, buf.getData(), pos, len );
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
		if( __s != null ) __s.close();
	}

}

private class SocketOutput extends haxe.io.Output {
	@:allow(sys.ssl.Socket) private var __s : Socket;

	public function new( s : Socket ) {
		this.__s = s;
	}

	public override function writeByte( c : Int ) {
		if( __s == null )
			throw "Invalid handle";
		try {
			__s.handshake();
			NativeSsl.ssl_send_char( @:privateAccess __s.ssl, c );
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw haxe.io.Error.Blocked;
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function writeBytes( buf : haxe.io.Bytes, pos : Int, len : Int) : Int {
		return try {
			__s.handshake();
			NativeSsl.ssl_send( @:privateAccess __s.ssl, buf.getData(), pos, len );
		} catch( e : Dynamic ) {
			if( e == "Blocking" )
				throw haxe.io.Error.Blocked;
			else
				throw haxe.io.Error.Custom(e);
		}
	}

	public override function close() {
		super.close();
		if( __s != null ) __s.close();
	}

}

@:coreApi
class Socket extends sys.net.Socket {
	
	public static var DEFAULT_VERIFY_CERT : Null<Bool> = true;

	public static var DEFAULT_CA : Null<Certificate>;
	
	private var conf : CONF;
	private var ssl : SSL;
	
	public var verifyCert : Null<Bool>;
	private var caCert : Null<Certificate>;
	private var hostname : String;

	private var ownCert : Null<Certificate>;
	private var ownKey : Null<Key>;
	private var altSNIContexts : Null<Array<{match: String->Bool, key: Key, cert: Certificate}>>;
	private var sniCallback : Dynamic;
	private var handshakeDone : Bool;

	private override function init() : Void {
		__s = NativeSocket.socket_new( false );
		input = new SocketInput( this );
		output = new SocketOutput( this );
		if( DEFAULT_VERIFY_CERT && DEFAULT_CA == null ){
			try {
				DEFAULT_CA = Certificate.loadDefaults();
			}catch( e : Dynamic ){}
		}	
		caCert = DEFAULT_CA;
		verifyCert = DEFAULT_VERIFY_CERT;
	}

	public override function connect(host : sys.net.Host, port : Int) : Void {
		try {
			conf = buildSSLConfig( false );
			ssl = NativeSsl.ssl_new( conf );
			handshakeDone = false;
			NativeSsl.ssl_set_socket( ssl, __s );
			if( hostname == null )
				hostname = host.host;
			if( hostname != null )
				NativeSsl.ssl_set_hostname( ssl, hostname );
			NativeSocket.socket_connect( __s, host.ip, port );
			handshake();
		} catch( s : String ) {
			if( s == "Invalid socket handle" )
				throw "Failed to connect on "+host.host+":"+port;
			else
				cpp.Lib.rethrow(s);
		} catch( e : Dynamic ) {
			cpp.Lib.rethrow(e);
		}
	}

	public function handshake() : Void {
		if( !handshakeDone ){
			try {
				NativeSsl.ssl_handshake( ssl );
				handshakeDone = true;
			} catch( e : Dynamic ) {
				if( e == "Blocking" )
					throw haxe.io.Error.Blocked;
				else
					cpp.Lib.rethrow( e );
			}
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

	public override function read() : String {
		handshake();
		var b = NativeSsl.ssl_read( ssl );
		if( b == null )
			return "";
		return haxe.io.Bytes.ofData(b).toString();
	}

	public override function write( content : String ) : Void {
		handshake();
		NativeSsl.ssl_write( ssl, haxe.io.Bytes.ofString(content).getData() );
	}

	public override function close() : Void {
		if( ssl != null ) NativeSsl.ssl_close( ssl );
		if( conf != null ) NativeSsl.conf_close( conf );
		if( altSNIContexts != null )
			sniCallback = null;
		NativeSocket.socket_close( __s );
		var input : SocketInput = cast input;
		var output : SocketOutput = cast output;
		@:privateAccess input.__s = output.__s = null;
		input.close();
		output.close();
	}

	public function addSNICertificate( cbServernameMatch : String->Bool, cert : Certificate, key : Key ) : Void {
		if( altSNIContexts == null )
			altSNIContexts = [];
		altSNIContexts.push( {match: cbServernameMatch, cert: cert, key: key} );
	}

	public override function bind( host : sys.net.Host, port : Int ) : Void {
		conf = buildSSLConfig( true );

		NativeSocket.socket_bind( __s, host.ip, port );
	}

	public override function accept() : Socket {
		var c = NativeSocket.socket_accept( __s );
		var ssl = NativeSsl.ssl_new( conf );
		NativeSsl.ssl_set_socket( ssl, c );

		var s = Type.createEmptyInstance( sys.ssl.Socket );
		s.__s = c;
		s.ssl = ssl;
		s.input = new SocketInput(s);
		s.output = new SocketOutput(s);
		s.handshakeDone = false;

		return s;
	}

	public function peerCertificate() : sys.ssl.Certificate {
		var x = NativeSsl.ssl_get_peer_certificate( ssl );
		return x==null ? null : new sys.ssl.Certificate( x );
	}

	private function buildSSLConfig( server : Bool ) : CONF {
		var conf : CONF = NativeSsl.conf_new( server );

		if( ownCert != null && ownKey != null )
			NativeSsl.conf_set_cert( conf, @:privateAccess ownCert.__x, @:privateAccess ownKey.__k );

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
			NativeSsl.conf_set_servername_callback( conf, sniCallback );
		}

		if ( caCert != null ) 
			NativeSsl.conf_set_ca( conf, caCert == null ? null : @:privateAccess caCert.__x  );
		if( verifyCert == null )
			NativeSsl.conf_set_verify( conf, 2 );
		else
			NativeSsl.conf_set_verify( conf, verifyCert ? 1 : 0 );
		
		return conf;
	}

	static function __init__() : Void {
		NativeSsl.init();
	}
}
