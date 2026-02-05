package cs.system.security.cryptography.x509certificates;

/** Defines the status of an X509 chain. */
@:native("System.Security.Cryptography.X509Certificates.X509ChainStatusFlags")
extern enum abstract X509ChainStatusFlags(Int) {
	var CtlNotSignatureValid = 262144;
	var CtlNotTimeValid = 131072;
	var CtlNotValidForUsage = 524288;
	var Cyclic = 128;
	var ExplicitDistrust = 67108864;
	var HasExcludedNameConstraint = 32768;
	var HasNotDefinedNameConstraint = 8192;
	var HasNotPermittedNameConstraint = 16384;
	var HasNotSupportedCriticalExtension = 134217728;
	var HasNotSupportedNameConstraint = 4096;
	var HasWeakSignature = 1048576;
	var InvalidBasicConstraints = 1024;
	var InvalidExtension = 256;
	var InvalidNameConstraints = 2048;
	var InvalidPolicyConstraints = 512;
	var NoError = 0;
	var NoIssuanceChainPolicy = 33554432;
	var NotSignatureValid = 8;
	var NotTimeNested = 2;
	var NotTimeValid = 1;
	var NotValidForUsage = 16;
	var OfflineRevocation = 16777216;
	var PartialChain = 65536;
	var RevocationStatusUnknown = 64;
	var Revoked = 4;
	var UntrustedRoot = 32;
	@:op(A | B) static function or(lhs:X509ChainStatusFlags, rhs:X509ChainStatusFlags):X509ChainStatusFlags;
	@:op(A & B) static function and(lhs:X509ChainStatusFlags, rhs:X509ChainStatusFlags):X509ChainStatusFlags;
	@:op(A ^ B) static function xor(lhs:X509ChainStatusFlags, rhs:X509ChainStatusFlags):X509ChainStatusFlags;
	@:op(~A) static function complement(value:X509ChainStatusFlags):X509ChainStatusFlags;
}
