package mbedtls;

@:native("mbedtls.SslAuthmode")
extern enum abstract SslAuthmode(Int) {
	var SSL_VERIFY_NONE;
	var SSL_VERIFY_OPTIONAL;
	var SSL_VERIFY_REQUIRED;
}
