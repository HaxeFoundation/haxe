package cs.system.security.cryptography;

@:native("System.Security.Cryptography.PbeEncryptionAlgorithm")
extern enum abstract PbeEncryptionAlgorithm(Int) {
	var Aes128Cbc = 1;
	var Aes192Cbc = 2;
	var Aes256Cbc = 3;
	var TripleDes3KeyPkcs12 = 4;
	var Unknown = 0;
}
