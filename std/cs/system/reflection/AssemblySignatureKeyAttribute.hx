package cs.system.reflection;

/** Provides migration from an older, simpler strong name key to a larger key with a stronger hashing algorithm. */
@:native("System.Reflection.AssemblySignatureKeyAttribute")
extern class AssemblySignatureKeyAttribute extends cs.system.Attribute {
	/**
	 * Gets the countersignature for the strong name for this assembly.
	 * @return The countersignature for this signature key.
	 */
	var Countersignature(default, never):String;
	/**
	 * Gets the public key for the strong name used to sign the assembly.
	 * @return The public key for this assembly.
	 */
	var PublicKey(default, never):String;
	function new(publicKey:String, countersignature:String):Void;
}
