package cs.system.security.cryptography;

/** Computes the  hash algorithm for the input data using the managed library. */
@:native("System.Security.Cryptography.SHA512Managed")
extern class SHA512Managed extends cs.system.security.cryptography.SHA512 {
	function new():Void;
	/** Initializes an instance of the  class using the managed library. */
	function Initialize():Void;
}
