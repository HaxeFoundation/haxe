package cs.system.security.cryptography;

/** Specifies flags that modify the behavior of the cryptographic service providers (CSP). */
@:native("System.Security.Cryptography.CspProviderFlags")
extern enum abstract CspProviderFlags(Int) {
	var CreateEphemeralKey = 128;
	var NoFlags = 0;
	var NoPrompt = 64;
	var UseArchivableKey = 16;
	var UseDefaultKeyContainer = 2;
	var UseExistingKey = 8;
	var UseMachineKeyStore = 1;
	var UseNonExportableKey = 4;
	var UseUserProtectedKey = 32;
	@:op(A | B) static function or(lhs:CspProviderFlags, rhs:CspProviderFlags):CspProviderFlags;
	@:op(A & B) static function and(lhs:CspProviderFlags, rhs:CspProviderFlags):CspProviderFlags;
	@:op(A ^ B) static function xor(lhs:CspProviderFlags, rhs:CspProviderFlags):CspProviderFlags;
	@:op(~A) static function complement(value:CspProviderFlags):CspProviderFlags;
}
