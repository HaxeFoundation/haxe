package cs.system.security.cryptography;

/** Creates a Digital Signature Algorithm () signature. */
@:native("System.Security.Cryptography.DSASignatureFormatter")
extern class DSASignatureFormatter extends cs.system.security.cryptography.AsymmetricSignatureFormatter {
	@:overload(function():Void {})
	function new(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
	/**
	 * Creates the Digital Signature Algorithm () PKCS #1 signature for the specified
	 * data.
	 * @param rgbHash The data to be signed.
	 * @return The digital signature for the specified data.
	 */
	function CreateSignature(rgbHash:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Specifies the hash algorithm for the Digital Signature Algorithm () signature
	 * formatter.
	 * @param strName The name of the hash algorithm to use for the signature
	 * formatter.
	 */
	function SetHashAlgorithm(strName:String):Void;
	/**
	 * Specifies the key to be used for the Digital Signature Algorithm () signature
	 * formatter.
	 * @param key The instance of  that holds the key.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
}
