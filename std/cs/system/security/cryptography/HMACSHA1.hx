package cs.system.security.cryptography;

/** Computes a Hash-based Message Authentication Code (HMAC) using the  hash function. */
@:native("System.Security.Cryptography.HMACSHA1")
extern class HMACSHA1 extends cs.system.security.cryptography.HMAC {
	@:overload(function():Void {})
	@:overload(function(key:cs.NativeArray<cs.UInt8>):Void {})
	function new(key:cs.NativeArray<cs.UInt8>, useManagedSha1:Bool):Void;
	/** Initializes an instance of . */
	function Initialize():Void;
}
