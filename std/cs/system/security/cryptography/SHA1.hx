package cs.system.security.cryptography;

/** Computes the  hash for the input data. */
@:native("System.Security.Cryptography.SHA1")
extern class SHA1 extends cs.system.security.cryptography.HashAlgorithm {
	@:overload(function():cs.system.security.cryptography.SHA1 {})
	/**
	 * Creates an instance of the default implementation of .
	 * @return A new instance of .
	 */
	static function Create(hashName:String):cs.system.security.cryptography.SHA1;
}
