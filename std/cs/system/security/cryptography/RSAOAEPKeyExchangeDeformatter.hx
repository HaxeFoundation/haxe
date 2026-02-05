package cs.system.security.cryptography;

/** Decrypts Optimal Asymmetric Encryption Padding (OAEP) key exchange data. */
@:native("System.Security.Cryptography.RSAOAEPKeyExchangeDeformatter")
extern class RSAOAEPKeyExchangeDeformatter extends cs.system.security.cryptography.AsymmetricKeyExchangeDeformatter {
	@:overload(function():Void {})
	function new(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	/**
	 * Extracts secret information from the encrypted key exchange data.
	 * @param rgbData The key exchange data within which the secret information is
	 * hidden.
	 * @return The secret information derived from the key exchange data.
	 */
	function DecryptKeyExchange(rgbData:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Sets the private key to use for decrypting the secret information.
	 * @param key The instance of the  algorithm that holds the private key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
}
