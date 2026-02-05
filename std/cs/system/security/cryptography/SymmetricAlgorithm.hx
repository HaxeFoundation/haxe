package cs.system.security.cryptography;

/** Represents the abstract base class from which all implementations of symmetric algorithms must inherit. */
@:native("System.Security.Cryptography.SymmetricAlgorithm")
extern class SymmetricAlgorithm {
	/**
	 * Gets or sets the block size, in bits, of the cryptographic operation.
	 * @return The block size, in bits.
	 */
	var BlockSize(default, default):Int;
	/**
	 * Gets or sets the feedback size, in bits, of the cryptographic operation.
	 * @return The feedback size in bits.
	 */
	var FeedbackSize(default, default):Int;
	/**
	 * Gets or sets the initialization vector () for the symmetric algorithm.
	 * @return The initialization vector.
	 */
	var IV(default, default):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets or sets the secret key for the symmetric algorithm.
	 * @return The secret key to use for the symmetric algorithm.
	 */
	var Key(default, default):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets or sets the size, in bits, of the secret key used by the symmetric
	 * algorithm.
	 * @return The size, in bits, of the secret key used by the symmetric algorithm.
	 */
	var KeySize(default, default):Int;
	/**
	 * Gets the block sizes, in bits, that are supported by the symmetric algorithm.
	 * @return An array that contains the block sizes supported by the algorithm.
	 */
	var LegalBlockSizes(default, never):cs.NativeArray<cs.system.security.cryptography.KeySizes>;
	/**
	 * Gets the key sizes, in bits, that are supported by the symmetric algorithm.
	 * @return An array that contains the key sizes supported by the algorithm.
	 */
	var LegalKeySizes(default, never):cs.NativeArray<cs.system.security.cryptography.KeySizes>;
	/**
	 * Gets or sets the mode for operation of the symmetric algorithm.
	 * @return The mode for operation of the symmetric algorithm. The default is .
	 */
	var Mode(default, default):cs.system.security.cryptography.CipherMode;
	/**
	 * Gets or sets the padding mode used in the symmetric algorithm.
	 * @return The padding mode used in the symmetric algorithm. The default is .
	 */
	var Padding(default, default):cs.system.security.cryptography.PaddingMode;
	@:overload(function():cs.system.security.cryptography.SymmetricAlgorithm {})
	/**
	 * Creates a default cryptographic object used to perform the symmetric algorithm.
	 * @return A default cryptographic object used to perform the symmetric algorithm.
	 */
	static function Create(algName:String):cs.system.security.cryptography.SymmetricAlgorithm;
	/** Releases all resources used by the  class. */
	function Clear():Void;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	/**
	 * Creates a symmetric decryptor object with the current  property and
	 * initialization vector ().
	 * @return A symmetric decryptor object.
	 */
	function CreateDecryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	/**
	 * Creates a symmetric encryptor object with the current  property and
	 * initialization vector ().
	 * @return A symmetric encryptor object.
	 */
	function CreateEncryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/** When overridden in a derived class, generates a random initialization vector () to use for the algorithm. */
	function GenerateIV():Void;
	/** When overridden in a derived class, generates a random key () to use for the algorithm. */
	function GenerateKey():Void;
	/**
	 * Determines whether the specified key size is valid for the current algorithm.
	 * @param bitLength The length, in bits, to check for a valid key size.
	 * @return if the specified key size is valid for the current algorithm; otherwise,
	 * .
	 */
	function ValidKeySize(bitLength:Int):Bool;
}
