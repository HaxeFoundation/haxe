package cs.system.security.cryptography;

/** Identifies Windows cryptographic object identifier (OID) groups. */
@:native("System.Security.Cryptography.OidGroup")
extern enum OidGroup {
	All;
	Attribute;
	EncryptionAlgorithm;
	EnhancedKeyUsage;
	ExtensionOrAttribute;
	HashAlgorithm;
	KeyDerivationFunction;
	Policy;
	PublicKeyAlgorithm;
	SignatureAlgorithm;
	Template;
}
