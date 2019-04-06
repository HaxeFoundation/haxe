package sys.ssl;

private typedef ConfigPtr = hl.Abstract<"mbedtls_ssl_config">;
private typedef ContextPtr = hl.Abstract<"mbedtls_ssl_context">;

@:keep class SNICbResult {
	public var cert : Certificate.CertificatePtr;
	public var key : Key.KeyPtr;
	public function new( cert : Certificate, key : Key ){
		this.cert = @:privateAccess cert.__x;
		this.key = @:privateAccess key.__k;
	}
}

@:hlNative("ssl","ssl_")
abstract Context(ContextPtr) {

	public function new(config) {
		this = ssl_new(config);
	}

	public function close() : Void {}
	public function handshake() : Int { return 0; }
	public function recvChar() : Int { return 0; }
	public function sendChar( c : Int ) : Int { return 0; }
	public function getPeerCertificate() : Certificate.CertificatePtr { return null; }
	public function recv( bytes : hl.Bytes, pos : Int, len : Int ) : Int { return 0; }
	public function send( bytes : hl.Bytes, pos : Int, len : Int ) : Int { return 0; }
	public function setSocket( socket : sys.net.Socket.SocketHandle ) : Void { }
	public function setHostname( name : hl.Bytes ) : Void { }

	@:hlNative("ssl","ssl_new") static function ssl_new( conf : Config ) : ContextPtr { return null; }

}

@:hlNative("ssl","conf_")
abstract Config(ConfigPtr) {

	public function new( server : Bool ) {
		this = conf_new(server);
	}

	public function setCert( cert : Certificate.CertificatePtr, pkey : Key.KeyPtr ) : Void { }
	public function setCa( ca : Certificate.CertificatePtr ) : Void { }
	public function close() : Void { }
	public function setVerify( mode : Int ) : Void { }
	public function setServernameCallback( cb : hl.Bytes -> SNICbResult ) : Void { }

	@:hlNative("ssl","conf_new") static function conf_new( server : Bool ) : ConfigPtr { return null; }

}
