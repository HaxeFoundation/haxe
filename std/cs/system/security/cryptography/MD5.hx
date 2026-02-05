package cs.system.security.cryptography;

/** Represents the abstract class from which all implementations of the  hash algorithm inherit. */
@:native("System.Security.Cryptography.MD5")
extern class MD5 extends cs.system.security.cryptography.HashAlgorithm {
	@:overload(function():cs.system.security.cryptography.MD5 {})
	/**
	 * Creates an instance of the default implementation of the  hash algorithm.
	 * @return A new instance of the  hash algorithm.
	 */
	static function Create(algName:String):cs.system.security.cryptography.MD5;
}
