package cs.system.security.cryptography.x509certificates;

/** Defines where and how to import the private key of an X.509 certificate. */
@:native("System.Security.Cryptography.X509Certificates.X509KeyStorageFlags")
extern enum abstract X509KeyStorageFlags(Int) {
	var DefaultKeySet = 0;
	var EphemeralKeySet = 32;
	var Exportable = 4;
	var MachineKeySet = 2;
	var PersistKeySet = 16;
	var UserKeySet = 1;
	var UserProtected = 8;
	@:op(A | B) static function or(lhs:X509KeyStorageFlags, rhs:X509KeyStorageFlags):X509KeyStorageFlags;
	@:op(A & B) static function and(lhs:X509KeyStorageFlags, rhs:X509KeyStorageFlags):X509KeyStorageFlags;
	@:op(A ^ B) static function xor(lhs:X509KeyStorageFlags, rhs:X509KeyStorageFlags):X509KeyStorageFlags;
	@:op(~A) static function complement(value:X509KeyStorageFlags):X509KeyStorageFlags;
}
