package cs.system.security.cryptography;

/** Represents the base class from which all asymmetric key exchange formatters derive. */
@:native("System.Security.Cryptography.AsymmetricKeyExchangeFormatter")
extern class AsymmetricKeyExchangeFormatter {
	/**
	 * When overridden in a derived class, gets the parameters for the asymmetric key
	 * exchange.
	 * @return A string in XML format containing the parameters of the asymmetric key
	 * exchange operation.
	 */
	var Parameters(default, never):String;
	@:overload(function(data:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	/**
	 * When overridden in a derived class, creates the encrypted key exchange data from
	 * the specified input data.
	 * @param data The secret information to be passed in the key exchange.
	 * @return The encrypted key exchange data to be sent to the intended recipient.
	 */
	function CreateKeyExchange(data:cs.NativeArray<cs.UInt8>, symAlgType:cs.system.Type):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, sets the public key to use for encrypting
	 * the secret information.
	 * @param key The instance of the implementation of  that holds the public key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
}
