package cs.system.security.cryptography;

/** Represents the base class from which all implementations of asymmetric signature formatters derive. */
@:native("System.Security.Cryptography.AsymmetricSignatureFormatter")
extern class AsymmetricSignatureFormatter {
	@:overload(function(rgbHash:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	/**
	 * When overridden in a derived class, creates the signature for the specified
	 * data.
	 * @param rgbHash The data to be signed.
	 * @return The digital signature for the  parameter.
	 */
	function CreateSignature(hash:cs.system.security.cryptography.HashAlgorithm):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, sets the hash algorithm to use for creating
	 * the signature.
	 * @param strName The name of the hash algorithm to use for creating the signature.
	 */
	function SetHashAlgorithm(strName:String):Void;
	/**
	 * When overridden in a derived class, sets the asymmetric algorithm to use to
	 * create the signature.
	 * @param key The instance of the implementation of  to use to create the
	 * signature.
	 */
	function SetKey(key:cs.system.security.cryptography.AsymmetricAlgorithm):Void;
}
