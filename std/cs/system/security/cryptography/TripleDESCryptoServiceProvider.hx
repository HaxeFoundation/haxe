package cs.system.security.cryptography;

/** Defines a wrapper object to access the cryptographic service provider (CSP) version of the  algorithm. This class cannot be inherited. */
@:native("System.Security.Cryptography.TripleDESCryptoServiceProvider")
extern class TripleDESCryptoServiceProvider extends cs.system.security.cryptography.TripleDES {
	function new():Void;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	function CreateDecryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	function CreateEncryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	/** Generates a random initialization vector () to use for the algorithm. */
	function GenerateIV():Void;
	/** Generates a random  to be used for the algorithm. */
	function GenerateKey():Void;
}
