package cs.system.security.cryptography;

@:native("System.Security.Cryptography.CryptographicOperations")
extern class CryptographicOperations {
	/**
	 * Determines the equality of two byte sequences in an amount of time that depends
	 * on the length of the sequences, but not their values.
	 * @param left The first buffer to compare.
	 * @param right The second buffer to compare.
	 * @return if  and  have the same values for  and the same contents; otherwise, .
	 */
	static function FixedTimeEquals(left:cs.system.ReadOnlySpan<cs.UInt8>, right:cs.system.ReadOnlySpan<cs.UInt8>):Bool;
	/** @param buffer  */
	static function ZeroMemory(buffer:cs.system.Span<cs.UInt8>):Void;
}
