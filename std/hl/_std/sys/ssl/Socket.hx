package sys.ssl;
import sys.ssl.Lib;
import sys.ssl.Key.KeyPtr;
import sys.ssl.Certificate.CertificatePtr;
import sys.net.Socket.SocketHandle;

private typedef ConfigPtr = hl.Abstract<"mbedtls_ssl_config">;
private typedef ContextPtr = hl.Abstract<"mbedtls_ssl_context">;

@:keep
private class SNICbResult {
	public var cert : CertificatePtr;
	public var key : KeyPtr; 
	public function new( cert : Certificate, key : Key ){
		this.cert = @:privateAccess cert.__x;
		this.key = @:privateAccess key.__k;
	}
}

private class SocketInput extends haxe.io.Input {
	@:allow(sys.ssl.Socket) private var __s : Socket;

	public function new( s : Socket ) {
		this.__s = s;
	}

	public override function readByte() {
		__s.handshake();
		var r = ssl_recv_char( @:privateAccess __s.ssl );
		if( r == -1 )
			throw haxe.io.Error.Blocked;
		else if( r < 0 )
			throw new haxe.io.Eof();
		return r;
	}

	public override function readBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) : Int {
		__s.handshake();
		var r = ssl_recv(  @:privateAccess __s.ssl, @:privateAccess buf.b, pos, len );
		if( r == -1 )
			throw haxe.io.Error.Blocked;
		else if( r < 0 )
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if( __s != null ) __s.close();
	}
	
	@:hlNative("ssl","ssl_recv") static function ssl_recv( ssl : ContextPtr, bytes : hl.Bytes, pos : Int, len : Int ) : Int { return -1; }
	@:hlNative("ssl","ssl_recv_char") static function ssl_recv_char( ssl : ContextPtr ) : Int { return -1; }
}

private class SocketOutput extends haxe.io.Output {
	@:allow(sys.ssl.Socket) private var __s : Socket;

	public function new( s : Socket ) {
		this.__s = s;
	}

	public override function writeByte( c : Int ) {
		__s.handshake();
		var r = ssl_send_char( @:privateAccess __s.ssl, c);
		if( r == -1 )
			throw haxe.io.Error.Blocked;
		else if( r < 0 )
			throw new haxe.io.Eof();
	}

	public override function writeBytes( buf : haxe.io.Bytes, pos : Int, len : Int) : Int {
		__s.handshake();
		var r = ssl_send( @:privateAccess __s.ssl, @:privateAccess buf.b, pos, len);
		if( r == -1 )
			throw haxe.io.Error.Blocked;
		else if( r < 0 )
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if( __s != null ) __s.close();
	}

	@:hlNative("ssl","ssl_send") static function ssl_send( ssl : ContextPtr, bytes : hl.Bytes, pos : Int, len : Int ) : Int { return -1; }
	@:hlNative("ssl","ssl_send_char") static function ssl_send_char( ssl : ContextPtr, c : Int ) : Int { return -1; }

}

@:coreApi @:access(sys.net.Socket)
class Socket extends sys.net.Socket {
	
	public static var DEFAULT_VERIFY_CERT : Null<Bool> = true;

	public static var DEFAULT_CA : Null<Certificate>;
	
	private var conf : ConfigPtr;
	private var ssl : ContextPtr;
	
	public var verifyCert : Null<Bool>;
	private var caCert : Null<Certificate>;
	private var hostname : String;

	private var ownCert : Null<Certificate>;
	private var ownKey : Null<Key>;
	private var altSNIContexts : Null<Array<{match: String->Bool, key: Key, cert: Certificate}>>;
	private var sniCallback : hl.Bytes -> SNICbResult;
	private var handshakeDone : Bool;

	private override function init() : Void {
		__s = sys.net.Socket.socket_new( false );
		input = new SocketInput( this );
		output = new SocketOutput( this );
		if( DEFAULT_VERIFY_CERT && DEFAULT_CA == null ){
			try {
				DEFAULT_CA = Certificate.loadDefaults();
			}catch( e : Dynamic ){}
		}
		verifyCert = DEFAULT_VERIFY_CERT;
		caCert = DEFAULT_CA;
	}

	public override function connect(host : sys.net.Host, port : Int) : Void {
		conf = buildConfig( false );
		ssl = ssl_new( conf );
		ssl_set_socket( ssl, __s );
		handshakeDone = false;
		if( hostname == null )
			hostname = host.host;
		if( hostname != null )
			ssl_set_hostname( ssl, @:privateAccess hostname.toUtf8() );
		if( !sys.net.Socket.socket_connect( __s, host.ip, port ) )
			throw new Sys.SysError("Failed to connect on "+host.toString()+":"+port);
		handshake();
	}

	public function handshake() : Void {
		if( !handshakeDone ){
			var r = ssl_handshake( ssl );
			if( r == 0 )
				handshakeDone = true;
			else if( r == -1 )
				throw haxe.io.Error.Blocked;
			else
				throw new haxe.io.Eof();
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

	public override function close() : Void {
		if( ssl != null ) ssl_close( ssl );
		if( conf != null ) conf_close( conf );
		if( altSNIContexts != null )
			sniCallback = null;
		sys.net.Socket.socket_close( __s );
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
		conf = buildConfig( true );

		sys.net.Socket.socket_bind( __s, host.ip, port );
	}

	public override function accept() : Socket {
		var c = sys.net.Socket.socket_accept( __s );
		var cssl = ssl_new( conf );
		ssl_set_socket( cssl, c );

		var s = Type.createEmptyInstance( sys.ssl.Socket );
		s.__s = c;
		s.ssl = cssl;
		s.input = new SocketInput(s);
		s.output = new SocketOutput(s);
		s.handshakeDone = false;

		return s;
	}

	public function peerCertificate() : sys.ssl.Certificate {
		var x = ssl_get_peer_certificate( ssl );
		return x==null ? null : new sys.ssl.Certificate( x );
	}

	private function buildConfig( server : Bool ) : ConfigPtr {
		var conf = conf_new( server );

		if( ownCert != null && ownKey != null )
			conf_set_cert( conf, @:privateAccess ownCert.__x, @:privateAccess ownKey.__k );

		if ( altSNIContexts != null ) {
			sniCallback = function(servername:hl.Bytes) : SNICbResult {
				var servername = @:privateAccess String.fromUTF8(servername);
				for( c in altSNIContexts ){
					if( c.match(servername) )
						return new SNICbResult(c.cert, c.key);
				}
				if( ownKey != null && ownCert != null )
					return new SNICbResult(ownCert, ownKey);
				return null;
			}
			conf_set_servername_callback( conf, sniCallback );
		}

		if ( caCert != null ) 
			conf_set_ca( conf, caCert == null ? null : @:privateAccess caCert.__x  );
		conf_set_verify( conf, if( verifyCert ) 1 else if( verifyCert==null ) 2 else 0 );
		
		return conf;
	}
	
	
	@:hlNative("ssl","ssl_new") static function ssl_new( conf : ConfigPtr ) : ContextPtr { return null; }
	@:hlNative("ssl","ssl_close") static function ssl_close( ssl : ContextPtr ) : Void {}
	@:hlNative("ssl","ssl_handshake") static function ssl_handshake( ssl : ContextPtr ) : Int { return -1; }
	@:hlNative("ssl","ssl_set_socket") static function ssl_set_socket( ssl : ContextPtr, socket : SocketHandle ) : Void { }
	@:hlNative("ssl","ssl_set_hostname") static function ssl_set_hostname( ssl : ContextPtr, name : hl.Bytes ) : Void { }
	@:hlNative("ssl","ssl_get_peer_certificate") static function ssl_get_peer_certificate( ssl : ContextPtr ) : CertificatePtr { return null; }
	
	@:hlNative("ssl","conf_new") static function conf_new( server : Bool ) : ConfigPtr { return null; }
	@:hlNative("ssl","conf_close") static function conf_close( conf : ConfigPtr ) : Void { }
	@:hlNative("ssl","conf_set_ca") static function conf_set_ca( conf : ConfigPtr, ca : CertificatePtr ) : Void { }
	@:hlNative("ssl","conf_set_verify") static function conf_set_verify( conf : ConfigPtr, mode : Int ) : Void { }
	@:hlNative("ssl","conf_set_cert") static function conf_set_cert( conf : ConfigPtr, cert : CertificatePtr, pkey : KeyPtr ) : Void { }
	@:hlNative("ssl","conf_set_servername_callback") static function conf_set_servername_callback( conf : ConfigPtr, cb : hl.Bytes -> SNICbResult ) : Void { }
	
}
