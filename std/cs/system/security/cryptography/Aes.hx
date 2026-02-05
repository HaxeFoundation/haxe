package cs.system.security.cryptography;

/** Represents the abstract base class from which all implementations of the Advanced Encryption Standard (AES) must inherit. */
@:native("System.Security.Cryptography.Aes")
extern class Aes extends cs.system.security.cryptography.SymmetricAlgorithm {
	@:overload(function():cs.system.security.cryptography.Aes {})
	/**
	 * Creates a cryptographic object that is used to perform the symmetric algorithm.
	 * @return A cryptographic object that is used to perform the symmetric algorithm.
	 */
	static function Create(algorithmName:String):cs.system.security.cryptography.Aes;
}
