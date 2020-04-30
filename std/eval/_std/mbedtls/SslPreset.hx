package mbedtls;

@:native("mbedtls.SslPreset")
extern enum abstract SslPreset(Int) {
	var SSL_PRESET_DEFAULT;
	var SSL_PRESET_SUITEB;
}
