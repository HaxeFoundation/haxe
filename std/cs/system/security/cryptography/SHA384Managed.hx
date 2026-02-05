package cs.system.security.cryptography;

/** Computes the  hash for the input data using the managed library. */
@:native("System.Security.Cryptography.SHA384Managed")
extern class SHA384Managed extends cs.system.security.cryptography.SHA384 {
	function new():Void;
	/** Initializes an instance of . */
	function Initialize():Void;
}
