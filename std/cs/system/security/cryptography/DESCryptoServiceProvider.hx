package cs.system.security.cryptography;

/** Defines a wrapper object to access the cryptographic service provider (CSP) version of the Data Encryption Standard () algorithm. This class cannot be inherited. */
@:native("System.Security.Cryptography.DESCryptoServiceProvider")
extern class DESCryptoServiceProvider extends cs.system.security.cryptography.DES {
	function new():Void;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	function CreateDecryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	function CreateEncryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	/** Generates a random initialization vector () to use for the algorithm. */
	function GenerateIV():Void;
	/** Generates a random key () to be used for the algorithm. */
	function GenerateKey():Void;
}
