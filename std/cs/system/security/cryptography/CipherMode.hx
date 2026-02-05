package cs.system.security.cryptography;

/** Specifies the block cipher mode to use for encryption. */
@:native("System.Security.Cryptography.CipherMode")
extern enum CipherMode {
	CBC;
	CFB;
	CTS;
	ECB;
	OFB;
}
