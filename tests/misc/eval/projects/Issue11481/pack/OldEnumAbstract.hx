package pack;

@:native("mbedtls.SslTransport")
extern enum abstract OldEnumAbstract(Int) {
	var SSL_TRANSPORT_STREAM;
	var SSL_TRANSPORT_DATAGRAM;

	macro static function f2() {
		return macro null;
	}

	public macro static function f1() {
		trace(SSL_TRANSPORT_STREAM);
		return macro null;
	}
}
