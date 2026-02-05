package cs.system.security.cryptography;

/** Defines a wrapper object to access the cryptographic service provider (CSP) implementation of the  algorithm. */
@:native("System.Security.Cryptography.SHA256CryptoServiceProvider")
extern class SHA256CryptoServiceProvider extends cs.system.security.cryptography.SHA256 {
	function new():Void;
	/** Initializes, or reinitializes, an instance of a hash algorithm. */
	function Initialize():Void;
}
