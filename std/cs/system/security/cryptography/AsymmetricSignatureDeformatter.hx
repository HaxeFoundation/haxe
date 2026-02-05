package cs.system.security.cryptography;

/** Represents the abstract base class from which all implementations of asymmetric signature deformatters derive. */
@:native("System.Security.Cryptography.AsymmetricSignatureDeformatter")
extern class AsymmetricSignatureDeformatter {
	/**
	 * When overridden in a derived class, sets the hash algorithm to use for verifying
	 * the signature.
	 * @param strName The name of the hash algorithm to use for verifying the
	 * signature.
	 */
	function SetHashAlgorithm(strName:String):Void;
	/**
	 * When overridden in a derived class, sets the public key to use for verifying the
	 * signature.
	 * @param key The instance of an implementation of  that holds the public key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	@:overload(function(rgbHash:cs.NativeArray<cs.UInt8>, rgbSignature:cs.NativeArray<cs.UInt8>):Bool {})
	/**
	 * When overridden in a derived class, verifies the signature for the specified
	 * data.
	 * @param rgbHash The data signed with .
	 * @param rgbSignature The signature to be verified for .
	 * @return if  matches the signature computed using the specified hash algorithm
	 * and key on ; otherwise, .
	 */
	function VerifySignature(hash:cs.system.security.cryptography.HashAlgorithm, rgbSignature:cs.NativeArray<cs.UInt8>):Bool;
}
