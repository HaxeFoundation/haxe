package cs.system.security.cryptography;

/** Specifies the type of padding to apply when the message data block is shorter than the full number of bytes needed for a cryptographic operation. */
@:native("System.Security.Cryptography.PaddingMode")
extern enum abstract PaddingMode(Int) {
	var ANSIX923 = 4;
	var ISO10126 = 5;
	var None = 1;
	var PKCS7 = 2;
	var Zeros = 3;
}
