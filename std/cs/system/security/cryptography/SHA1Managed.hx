package cs.system.security.cryptography;

/** Computes the  hash for the input data using the managed library. */
@:native("System.Security.Cryptography.SHA1Managed")
extern class SHA1Managed extends cs.system.security.cryptography.SHA1 {
	function new():Void;
	/** Initializes an instance of . */
	function Initialize():Void;
}
