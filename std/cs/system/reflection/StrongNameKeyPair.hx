package cs.system.reflection;

/** Encapsulates access to a public or private key pair used to sign strong name assemblies. */
@:native("System.Reflection.StrongNameKeyPair")
extern class StrongNameKeyPair {
	/**
	 * Gets the public part of the public key or public key token of the key pair.
	 * @return An array of type  containing the public key or public key token of the
	 * key pair.
	 */
	var PublicKey(default, never):cs.NativeArray<cs.UInt8>;
	@:overload(function(keyPairArray:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(keyPairFile:cs.system.io.FileStream):Void {})
	function new(keyPairContainer:String):Void;
}
