package cs.system.security.cryptography.x509certificates;

/** Specifies conditions under which verification of certificates in the X509 chain should be conducted. */
@:native("System.Security.Cryptography.X509Certificates.X509VerificationFlags")
extern enum abstract X509VerificationFlags(Int) {
	var AllFlags = 4095;
	var AllowUnknownCertificateAuthority = 16;
	var IgnoreCertificateAuthorityRevocationUnknown = 1024;
	var IgnoreCtlNotTimeValid = 2;
	var IgnoreCtlSignerRevocationUnknown = 512;
	var IgnoreEndRevocationUnknown = 256;
	var IgnoreInvalidBasicConstraints = 8;
	var IgnoreInvalidName = 64;
	var IgnoreInvalidPolicy = 128;
	var IgnoreNotTimeNested = 4;
	var IgnoreNotTimeValid = 1;
	var IgnoreRootRevocationUnknown = 2048;
	var IgnoreWrongUsage = 32;
	var NoFlag = 0;
	@:op(A | B) static function or(lhs:X509VerificationFlags, rhs:X509VerificationFlags):X509VerificationFlags;
	@:op(A & B) static function and(lhs:X509VerificationFlags, rhs:X509VerificationFlags):X509VerificationFlags;
	@:op(A ^ B) static function xor(lhs:X509VerificationFlags, rhs:X509VerificationFlags):X509VerificationFlags;
	@:op(~A) static function complement(value:X509VerificationFlags):X509VerificationFlags;
}
