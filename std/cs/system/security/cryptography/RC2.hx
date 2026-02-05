package cs.system.security.cryptography;

/** Represents the base class from which all implementations of the  algorithm must derive. */
@:native("System.Security.Cryptography.RC2")
extern class RC2 extends cs.system.security.cryptography.SymmetricAlgorithm {
	/**
	 * Gets or sets the effective size of the secret key used by the  algorithm in
	 * bits.
	 * @return The effective key size used by the  algorithm.
	 */
	var EffectiveKeySize(default, default):Int;
	@:overload(function():cs.system.security.cryptography.RC2 {})
	/**
	 * Creates an instance of a cryptographic object to perform the  algorithm.
	 * @return An instance of a cryptographic object.
	 */
	static function Create(AlgName:String):cs.system.security.cryptography.RC2;
}
