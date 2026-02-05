package cs.system.security.cryptography.x509certificates;

/** Defines how the certificate key can be used. If this value is not defined, the key can be used for any purpose. */
@:native("System.Security.Cryptography.X509Certificates.X509KeyUsageFlags")
extern enum abstract X509KeyUsageFlags(Int) {
	var CrlSign = 2;
	var DataEncipherment = 16;
	var DecipherOnly = 32768;
	var DigitalSignature = 128;
	var EncipherOnly = 1;
	var KeyAgreement = 8;
	var KeyCertSign = 4;
	var KeyEncipherment = 32;
	var None = 0;
	var NonRepudiation = 64;
	@:op(A | B) static function or(lhs:X509KeyUsageFlags, rhs:X509KeyUsageFlags):X509KeyUsageFlags;
	@:op(A & B) static function and(lhs:X509KeyUsageFlags, rhs:X509KeyUsageFlags):X509KeyUsageFlags;
	@:op(A ^ B) static function xor(lhs:X509KeyUsageFlags, rhs:X509KeyUsageFlags):X509KeyUsageFlags;
	@:op(~A) static function complement(value:X509KeyUsageFlags):X509KeyUsageFlags;
}
