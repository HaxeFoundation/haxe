package cs.system.security.cryptography;

@:native("System.Security.Cryptography.PbeEncryptionAlgorithm")
extern enum PbeEncryptionAlgorithm {
	Aes128Cbc;
	Aes192Cbc;
	Aes256Cbc;
	TripleDes3KeyPkcs12;
	Unknown;
}
