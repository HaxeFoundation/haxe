package cs.system.security.cryptography;

/** Creates an  PKCS #1 version 1.5 signature. */
@:native("System.Security.Cryptography.RSAPKCS1SignatureFormatter")
extern class RSAPKCS1SignatureFormatter extends cs.system.security.cryptography.AsymmetricSignatureFormatter {
	@:overload(function():Void {})
	function new(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	/**
	 * Creates the  PKCS #1 signature for the specified data.
	 * @param rgbHash The data to be signed.
	 * @return The digital signature for .
	 */
	function CreateSignature(rgbHash:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Sets the hash algorithm to use for creating the signature.
	 * @param strName The name of the hash algorithm to use for creating the signature.
	 */
	function SetHashAlgorithm(strName:String):Void;
	/**
	 * Sets the private key to use for creating the signature.
	 * @param key The instance of the  algorithm that holds the private key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
}
