package cs.system.security.cryptography;

/** Accesses the managed version of the  algorithm. This class cannot be inherited. */
@:native("System.Security.Cryptography.RijndaelManaged")
extern class RijndaelManaged extends cs.system.security.cryptography.Rijndael {
	function new():Void;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	function CreateDecryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	function CreateEncryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	/** Generates a random initialization vector () to be used for the algorithm. */
	function GenerateIV():Void;
	/** Generates a random  to be used for the algorithm. */
	function GenerateKey():Void;
}
