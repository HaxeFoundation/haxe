package cs.system.security.cryptography;

/** Decrypts the PKCS #1 key exchange data. */
@:native("System.Security.Cryptography.RSAPKCS1KeyExchangeDeformatter")
extern class RSAPKCS1KeyExchangeDeformatter extends cs.system.security.cryptography.AsymmetricKeyExchangeDeformatter {
	/**
	 * Gets or sets the random number generator algorithm to use in the creation of the
	 * key exchange.
	 * @return The instance of a random number generator algorithm to use.
	 */
	var RNG(default, default):cs.system.security.cryptography.RandomNumberGenerator;
	@:overload(function():Void {})
	function new(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	/**
	 * Extracts secret information from the encrypted key exchange data.
	 * @param rgbIn The key exchange data within which the secret information is
	 * hidden.
	 * @return The secret information derived from the key exchange data.
	 */
	function DecryptKeyExchange(rgbIn:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Sets the private key to use for decrypting the secret information.
	 * @param key The instance of the  algorithm that holds the private key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
}
