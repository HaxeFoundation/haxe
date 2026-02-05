package cs.system.security.cryptography;

/** Specifies the block cipher mode to use for encryption. */
@:native("System.Security.Cryptography.CipherMode")
extern enum abstract CipherMode(Int) {
	var CBC = 1;
	var CFB = 4;
	var CTS = 5;
	var ECB = 2;
	var OFB = 3;
}
