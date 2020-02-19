package mbedtls;

@:native("mbedtls.SslEndpoint")
extern enum abstract SslEndpoint(Int) {
	var SSL_IS_CLIENT;
	var SSL_IS_SERVER;
}
