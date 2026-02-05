package cs.system.security.cryptography;

/** Represents the base class from which all implementations of the  symmetric encryption algorithm must inherit. */
@:native("System.Security.Cryptography.Rijndael")
extern class Rijndael extends cs.system.security.cryptography.SymmetricAlgorithm {
	@:overload(function():cs.system.security.cryptography.Rijndael {})
	/**
	 * Creates a cryptographic object to perform the  algorithm.
	 * @return A cryptographic object.
	 */
	static function Create(algName:String):cs.system.security.cryptography.Rijndael;
}
