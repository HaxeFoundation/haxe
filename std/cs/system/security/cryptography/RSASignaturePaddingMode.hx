package cs.system.security.cryptography;

/** Specifies the padding mode to use with RSA signature creation or verification operations. */
@:native("System.Security.Cryptography.RSASignaturePaddingMode")
extern enum abstract RSASignaturePaddingMode(Int) {
	var Pkcs1 = 0;
	var Pss = 1;
}
