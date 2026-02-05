package cs.system.security.cryptography;

/** Represents the base class for Triple Data Encryption Standard algorithms from which all  implementations must derive. */
@:native("System.Security.Cryptography.TripleDES")
extern class TripleDES extends cs.system.security.cryptography.SymmetricAlgorithm {
	@:overload(function():cs.system.security.cryptography.TripleDES {})
	/**
	 * Creates an instance of a cryptographic object to perform the  algorithm.
	 * @return An instance of a cryptographic object.
	 */
	static function Create(str:String):cs.system.security.cryptography.TripleDES;
	/**
	 * Determines whether the specified key is weak.
	 * @param rgbKey The secret key to test for weakness.
	 * @return if the key is weak; otherwise, .
	 */
	static function IsWeakKey(rgbKey:cs.NativeArray<cs.UInt8>):Bool;
}
