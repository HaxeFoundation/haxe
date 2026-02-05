package cs.system.security.cryptography;

/** Specifies the padding mode to use with RSA encryption or decryption operations. */
@:native("System.Security.Cryptography.RSAEncryptionPaddingMode")
extern enum RSAEncryptionPaddingMode {
	Oaep;
	Pkcs1;
}
