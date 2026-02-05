package cs.system.security.cryptography;

/** Specifies the mode of a cryptographic stream. */
@:native("System.Security.Cryptography.CryptoStreamMode")
extern enum abstract CryptoStreamMode(Int) {
	var Read = 0;
	var Write = 1;
}
