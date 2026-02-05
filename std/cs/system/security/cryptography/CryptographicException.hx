package cs.system.security.cryptography;

/** The exception that is thrown when an error occurs during a cryptographic operation. */
@:native("System.Security.Cryptography.CryptographicException")
extern class CryptographicException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(hr:Int):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(format:String, insert:String):Void;
}
