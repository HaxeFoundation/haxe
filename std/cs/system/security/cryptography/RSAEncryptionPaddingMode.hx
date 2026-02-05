package cs.system.security.cryptography;

/** Specifies the padding mode to use with RSA encryption or decryption operations. */
@:native("System.Security.Cryptography.RSAEncryptionPaddingMode")
extern enum abstract RSAEncryptionPaddingMode(Int) {
	var Oaep = 1;
	var Pkcs1 = 0;
}
