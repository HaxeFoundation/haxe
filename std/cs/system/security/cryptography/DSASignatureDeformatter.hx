package cs.system.security.cryptography;

/** Verifies a Digital Signature Algorithm () PKCS#1 v1.5 signature. */
@:native("System.Security.Cryptography.DSASignatureDeformatter")
extern class DSASignatureDeformatter extends cs.system.security.cryptography.AsymmetricSignatureDeformatter {
	@:overload(function():Void {})
	function new(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	/**
	 * Specifies the hash algorithm for the Digital Signature Algorithm () signature
	 * deformatter.
	 * @param strName The name of the hash algorithm to use for the signature
	 * deformatter.
	 */
	function SetHashAlgorithm(strName:String):Void;
	/**
	 * Specifies the key to be used for the Digital Signature Algorithm () signature
	 * deformatter.
	 * @param key The instance of  that holds the key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	/**
	 * Verifies the Digital Signature Algorithm () signature on the data.
	 * @param rgbHash The data signed with .
	 * @param rgbSignature The signature to be verified for .
	 * @return if the signature is valid for the data; otherwise, .
	 */
	function VerifySignature(rgbHash:cs.NativeArray<cs.UInt8>, rgbSignature:cs.NativeArray<cs.UInt8>):Bool;
}
