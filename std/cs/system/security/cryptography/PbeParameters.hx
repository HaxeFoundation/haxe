package cs.system.security.cryptography;

@:native("System.Security.Cryptography.PbeParameters")
extern class PbeParameters {
	var EncryptionAlgorithm(default, never):cs.system.security.cryptography.PbeEncryptionAlgorithm;
	var HashAlgorithm(default, never):cs.system.security.cryptography.HashAlgorithmName;
	var IterationCount(default, never):Int;
	function new(encryptionAlgorithm:cs.system.security.cryptography.PbeEncryptionAlgorithm, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, iterationCount:Int):Void;
}
