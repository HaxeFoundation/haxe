package cs.system.security.cryptography;

/** Computes a Hash-based Message Authentication Code (HMAC) by using the  hash function. */
@:native("System.Security.Cryptography.HMACSHA256")
extern class HMACSHA256 extends cs.system.security.cryptography.HMAC {
	@:overload(function():Void {})
	function new(key:cs.NativeArray<cs.UInt8>):Void;
	function Initialize():Void;
}
