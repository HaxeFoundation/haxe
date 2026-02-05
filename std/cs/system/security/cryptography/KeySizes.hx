package cs.system.security.cryptography;

/** Determines the set of valid key sizes for the symmetric cryptographic algorithms. */
@:native("System.Security.Cryptography.KeySizes")
extern class KeySizes {
	/**
	 * Specifies the maximum key size in bits.
	 * @return The maximum key size in bits.
	 */
	var MaxSize(default, never):Int;
	/**
	 * Specifies the minimum key size in bits.
	 * @return The minimum key size in bits.
	 */
	var MinSize(default, never):Int;
	/**
	 * Specifies the interval between valid key sizes in bits.
	 * @return The interval between valid key sizes in bits.
	 */
	var SkipSize(default, never):Int;
	function new(minSize:Int, maxSize:Int, skipSize:Int):Void;
}
