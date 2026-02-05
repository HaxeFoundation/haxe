package cs.system.security.cryptography;

/** Represents the abstract class from which all implementations of Hash-based Message Authentication Code (HMAC) must derive. */
@:native("System.Security.Cryptography.HMAC")
extern class HMAC extends cs.system.security.cryptography.KeyedHashAlgorithm {
	/**
	 * Gets or sets the block size to use in the hash value.
	 * @return The block size to use in the hash value.
	 */
	var BlockSizeValue(default, default):Int;
	/**
	 * Gets or sets the name of the hash algorithm to use for hashing.
	 * @return The name of the hash algorithm.
	 */
	var HashName(default, default):String;
	@:overload(function():cs.system.security.cryptography.HMAC {})
	/**
	 * Creates an instance of the default implementation of a Hash-based Message
	 * Authentication Code (HMAC).
	 * @return A new SHA-1 instance, unless the default settings have been changed by
	 * using the <cryptoClass> element.
	 */
	static function Create(algorithmName:String):cs.system.security.cryptography.HMAC;
	/** Initializes an instance of the default implementation of . */
	function Initialize():Void;
}
