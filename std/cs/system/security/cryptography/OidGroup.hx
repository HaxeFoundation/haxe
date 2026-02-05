package cs.system.security.cryptography;

/** Identifies Windows cryptographic object identifier (OID) groups. */
@:native("System.Security.Cryptography.OidGroup")
extern enum abstract OidGroup(Int) {
	var All = 0;
	var Attribute = 5;
	var EncryptionAlgorithm = 2;
	var EnhancedKeyUsage = 7;
	var ExtensionOrAttribute = 6;
	var HashAlgorithm = 1;
	var KeyDerivationFunction = 10;
	var Policy = 8;
	var PublicKeyAlgorithm = 3;
	var SignatureAlgorithm = 4;
	var Template = 9;
}
