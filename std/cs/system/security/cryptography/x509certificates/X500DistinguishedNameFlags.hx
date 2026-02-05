package cs.system.security.cryptography.x509certificates;

/** Specifies characteristics of the X.500 distinguished name. */
@:native("System.Security.Cryptography.X509Certificates.X500DistinguishedNameFlags")
extern enum abstract X500DistinguishedNameFlags(Int) {
	var DoNotUsePlusSign = 32;
	var DoNotUseQuotes = 64;
	var ForceUTF8Encoding = 16384;
	var None = 0;
	var Reversed = 1;
	var UseCommas = 128;
	var UseNewLines = 256;
	var UseSemicolons = 16;
	var UseT61Encoding = 8192;
	var UseUTF8Encoding = 4096;
	@:op(A | B) static function or(lhs:X500DistinguishedNameFlags, rhs:X500DistinguishedNameFlags):X500DistinguishedNameFlags;
	@:op(A & B) static function and(lhs:X500DistinguishedNameFlags, rhs:X500DistinguishedNameFlags):X500DistinguishedNameFlags;
	@:op(A ^ B) static function xor(lhs:X500DistinguishedNameFlags, rhs:X500DistinguishedNameFlags):X500DistinguishedNameFlags;
	@:op(~A) static function complement(value:X500DistinguishedNameFlags):X500DistinguishedNameFlags;
}
