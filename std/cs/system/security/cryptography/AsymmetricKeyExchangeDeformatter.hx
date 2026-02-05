package cs.system.security.cryptography;

/** Represents the base class from which all asymmetric key exchange deformatters derive. */
@:native("System.Security.Cryptography.AsymmetricKeyExchangeDeformatter")
extern class AsymmetricKeyExchangeDeformatter {
	/**
	 * When overridden in a derived class, gets or sets the parameters for the
	 * asymmetric key exchange.
	 * @return A string in XML format containing the parameters of the asymmetric key
	 * exchange operation.
	 */
	var Parameters(default, default):String;
	/**
	 * When overridden in a derived class, extracts secret information from the
	 * encrypted key exchange data.
	 * @param rgb The key exchange data within which the secret information is hidden.
	 * @return The secret information derived from the key exchange data.
	 */
	function DecryptKeyExchange(rgb:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, sets the private key to use for decrypting
	 * the secret information.
	 * @param key The instance of the implementation of  that holds the private key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
}
