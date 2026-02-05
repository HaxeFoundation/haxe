package cs.system.security.cryptography;

/** Creates Optimal Asymmetric Encryption Padding (OAEP) key exchange data using . */
@:native("System.Security.Cryptography.RSAOAEPKeyExchangeFormatter")
extern class RSAOAEPKeyExchangeFormatter extends cs.system.security.cryptography.AsymmetricKeyExchangeFormatter {
	/**
	 * Gets or sets the parameter used to create padding in the key exchange creation
	 * process.
	 * @return The parameter value.
	 */
	var Parameter(default, default):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets or sets the random number generator algorithm to use in the creation of the
	 * key exchange.
	 * @return The instance of a random number generator algorithm to use.
	 */
	var Rng(default, default):cs.system.security.cryptography.RandomNumberGenerator;
	@:overload(function():Void {})
	function new(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	@:overload(function(rgbData:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	/**
	 * Creates the encrypted key exchange data from the specified input data.
	 * @param rgbData The secret information to be passed in the key exchange.
	 * @return The encrypted key exchange data to be sent to the intended recipient.
	 */
	function CreateKeyExchange(rgbData:cs.NativeArray<cs.UInt8>, symAlgType:cs.system.Type):cs.NativeArray<cs.UInt8>;
	/**
	 * Sets the public key to use for encrypting the key exchange data.
	 * @param key The instance of the  algorithm that holds the public key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
}
