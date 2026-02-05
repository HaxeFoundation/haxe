package cs.system.security.cryptography;

/** The exception that is thrown when an unexpected operation occurs during a cryptographic operation. */
@:native("System.Security.Cryptography.CryptographicUnexpectedOperationException")
extern class CryptographicUnexpectedOperationException extends cs.system.security.cryptography.CryptographicException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(format:String, insert:String):Void;
}
