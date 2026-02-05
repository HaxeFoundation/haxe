package cs.system.security.cryptography;

/** Represents the abstract class from which all implementations of keyed hash algorithms must derive. */
@:native("System.Security.Cryptography.KeyedHashAlgorithm")
extern class KeyedHashAlgorithm extends cs.system.security.cryptography.HashAlgorithm {
	/**
	 * Gets or sets the key to use in the hash algorithm.
	 * @return The key to use in the hash algorithm.
	 */
	var Key(default, default):cs.NativeArray<cs.UInt8>;
	@:overload(function():cs.system.security.cryptography.KeyedHashAlgorithm {})
	/**
	 * Creates an instance of the default implementation of a keyed hash algorithm.
	 * @return A new  instance, unless the default settings have been changed.
	 */
	static function Create(algName:String):cs.system.security.cryptography.KeyedHashAlgorithm;
}
