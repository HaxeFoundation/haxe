package cs.system.security.cryptography;

/** Computes the  hash value for the input data using the implementation provided by the cryptographic service provider (CSP). This class cannot be inherited. */
@:native("System.Security.Cryptography.SHA1CryptoServiceProvider")
extern class SHA1CryptoServiceProvider extends cs.system.security.cryptography.SHA1 {
	function new():Void;
	/** Initializes an instance of . */
	function Initialize():Void;
}
