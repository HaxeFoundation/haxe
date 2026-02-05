package cs.system.security.cryptography;

/** Computes the  hash for the input data using the managed library. */
@:native("System.Security.Cryptography.SHA256Managed")
extern class SHA256Managed extends cs.system.security.cryptography.SHA256 {
	function new():Void;
	/** Initializes an instance of . */
	function Initialize():Void;
}
