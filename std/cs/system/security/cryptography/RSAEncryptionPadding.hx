package cs.system.security.cryptography;

/** Specifies the padding mode and parameters to use with RSA encryption or decryption operations. */
@:native("System.Security.Cryptography.RSAEncryptionPadding")
extern class RSAEncryptionPadding {
	/**
	 * Gets an object that represents the Optimal Asymmetric Encryption Padding (OAEP)
	 * encryption standard with a SHA1 hash algorithm.
	 * @return An object that represents the OAEP encryption standard with a SHA1 hash
	 * algorithm.
	 */
	static var OaepSHA1(default, never):cs.system.security.cryptography.RSAEncryptionPadding;
	/**
	 * Gets an object that represents the Optimal Asymmetric Encryption Padding (OAEP)
	 * encryption standard with a SHA256 hash algorithm.
	 * @return An object that represents the OAEP encryption standard with a SHA256
	 * hash algorithm.
	 */
	static var OaepSHA256(default, never):cs.system.security.cryptography.RSAEncryptionPadding;
	/**
	 * Gets an object that represents the Optimal Asymmetric Encryption Padding (OAEP)
	 * encryption standard with a SHA-384 hash algorithm.
	 * @return An object that represents the OAEP encryption standard with a SHA384
	 * hash algorithm.
	 */
	static var OaepSHA384(default, never):cs.system.security.cryptography.RSAEncryptionPadding;
	/**
	 * Gets an object that represents the Optimal Asymmetric Encryption Padding (OAEP)
	 * encryption standard with a SHA512 hash algorithm.
	 * @return An object that represents the OAEP encryption standard with a SHA512
	 * hash algorithm.
	 */
	static var OaepSHA512(default, never):cs.system.security.cryptography.RSAEncryptionPadding;
	/**
	 * Gets an object that represents the PKCS #1 encryption standard.
	 * @return An object that represents the PKCS #1 encryption standard.
	 */
	static var Pkcs1(default, never):cs.system.security.cryptography.RSAEncryptionPadding;
	/**
	 * Gets the padding mode represented by this  instance.
	 * @return A padding mode.
	 */
	var Mode(default, never):cs.system.security.cryptography.RSAEncryptionPaddingMode;
	/**
	 * Gets the hash algorithm used in conjunction with the  padding mode.
	 * @return The hash algorithm.
	 */
	var OaepHashAlgorithm(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Creates a new  instance whose  is  with the given hash algorithm.
	 * @param hashAlgorithm The hash algorithm.
	 * @return An object whose mode is  is  with the hash algorithm specified by .
	 */
	static function CreateOaep(hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.system.security.cryptography.RSAEncryptionPadding;
	/**
	 * Indicates whether two specified  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.security.cryptography.RSAEncryptionPadding, right:cs.system.security.cryptography.RSAEncryptionPadding):Bool;
	/**
	 * Indicates whether two specified  objects are unequal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.security.cryptography.RSAEncryptionPadding, right:cs.system.security.cryptography.RSAEncryptionPadding):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Determines whether the current instance is equal to the specified object.
	 * @param obj The object to compare.
	 * @return if  is equal to the current instance; otherwise, .
	 */
	function Equals(other:cs.system.security.cryptography.RSAEncryptionPadding):Bool;
	/**
	 * Returns the hash code of this  object.
	 * @return The hash code of this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string representation of the current  instance.
	 * @return The string representation of the current object.
	 */
	function ToString():String;
}
