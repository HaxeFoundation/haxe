package cs.system.security.cryptography;

/** Represents the abstract base class from which all classes that derive byte sequences of a specified length inherit. */
@:native("System.Security.Cryptography.DeriveBytes")
extern class DeriveBytes {
	/** When overridden in a derived class, releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * When overridden in a derived class, returns pseudo-random key bytes.
	 * @param cb The number of pseudo-random key bytes to generate.
	 * @return A byte array filled with pseudo-random key bytes.
	 */
	function GetBytes(cb:Int):cs.NativeArray<cs.UInt8>;
	/** When overridden in a derived class, resets the state of the operation. */
	function Reset():Void;
}
