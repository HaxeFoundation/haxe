package cs.system.security.cryptography;

/** Represents the base class for the Data Encryption Standard (DES) algorithm from which all  implementations must derive. */
@:native("System.Security.Cryptography.DES")
extern class DES extends cs.system.security.cryptography.SymmetricAlgorithm {
	@:overload(function():cs.system.security.cryptography.DES {})
	/**
	 * Creates an instance of a cryptographic object to perform the Data Encryption
	 * Standard () algorithm.
	 * @return A cryptographic object.
	 */
	static function Create(algName:String):cs.system.security.cryptography.DES;
	/**
	 * Determines whether the specified key is semi-weak.
	 * @param rgbKey The secret key to test for semi-weakness.
	 * @return if the key is semi-weak; otherwise, .
	 */
	static function IsSemiWeakKey(rgbKey:cs.NativeArray<cs.UInt8>):Bool;
	/**
	 * Determines whether the specified key is weak.
	 * @param rgbKey The secret key to test for weakness.
	 * @return if the key is weak; otherwise, .
	 */
	static function IsWeakKey(rgbKey:cs.NativeArray<cs.UInt8>):Bool;
}
