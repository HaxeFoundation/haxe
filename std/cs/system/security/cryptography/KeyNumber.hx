package cs.system.security.cryptography;

/** Specifies whether to create an asymmetric signature key or an asymmetric exchange key. */
@:native("System.Security.Cryptography.KeyNumber")
extern enum KeyNumber {
	Exchange;
	Signature;
}
