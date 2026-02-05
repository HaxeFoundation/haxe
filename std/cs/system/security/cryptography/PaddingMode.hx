package cs.system.security.cryptography;

/** Specifies the type of padding to apply when the message data block is shorter than the full number of bytes needed for a cryptographic operation. */
@:native("System.Security.Cryptography.PaddingMode")
extern enum PaddingMode {
	ANSIX923;
	ISO10126;
	None;
	PKCS7;
	Zeros;
}
