package cs.system.security.cryptography;

/** Verifies an  PKCS #1 version 1.5 signature. */
@:native("System.Security.Cryptography.RSAPKCS1SignatureDeformatter")
extern class RSAPKCS1SignatureDeformatter extends cs.system.security.cryptography.AsymmetricSignatureDeformatter {
	@:overload(function():Void {})
	function new(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	/**
	 * Sets the hash algorithm to use for verifying the signature.
	 * @param strName The name of the hash algorithm to use for verifying the
	 * signature.
	 */
	function SetHashAlgorithm(strName:String):Void;
	/**
	 * Sets the public key to use for verifying the signature.
	 * @param key The instance of  that holds the public key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	/**
	 * Verifies the  PKCS#1 signature for the specified data.
	 * @param rgbHash The data signed with .
	 * @param rgbSignature The signature to be verified for .
	 * @return if  matches the signature computed using the specified hash algorithm
	 * and key on ; otherwise, .
	 */
	function VerifySignature(rgbHash:cs.NativeArray<cs.UInt8>, rgbSignature:cs.NativeArray<cs.UInt8>):Bool;
}
