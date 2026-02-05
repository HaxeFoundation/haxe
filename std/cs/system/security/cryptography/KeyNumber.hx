package cs.system.security.cryptography;

/** Specifies whether to create an asymmetric signature key or an asymmetric exchange key. */
@:native("System.Security.Cryptography.KeyNumber")
extern enum abstract KeyNumber(Int) {
	var Exchange = 1;
	var Signature = 2;
}
