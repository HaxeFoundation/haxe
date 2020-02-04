package mbedtls;

@:native("mbedtls.SslTransport")
extern enum abstract SslTransport(Int) {
	var SSL_TRANSPORT_STREAM;
	var SSL_TRANSPORT_DATAGRAM;
}
