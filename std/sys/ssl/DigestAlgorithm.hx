package sys.ssl;

@:enum
abstract DigestAlgorithm(String) to String {
	var MD5 = "MD5";
	var SHA1 = "SHA1";
	var SHA224 = "SHA224";
	var SHA256 = "SHA256";
	var SHA384 = "SHA384";
	var SHA512 = "SHA512";
	var RIPEMD160 = "RIPEMD160";
}
