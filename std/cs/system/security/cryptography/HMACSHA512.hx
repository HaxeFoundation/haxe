package cs.system.security.cryptography;

/** Computes a Hash-based Message Authentication Code (HMAC) using the  hash function. */
@:native("System.Security.Cryptography.HMACSHA512")
extern class HMACSHA512 extends cs.system.security.cryptography.HMAC {
	/**
	 * Provides a workaround for the .NET Framework 2.0 implementation of the 
	 * algorithm, which is inconsistent with the .NET Framework 2.0 Service Pack 1
	 * implementation.
	 * @return to enable .NET Framework 2.0 Service Pack 1 applications to interact
	 * with .NET Framework 2.0 applications; otherwise, .
	 */
	var ProduceLegacyHmacValues(default, default):Bool;
	@:overload(function():Void {})
	function new(key:cs.NativeArray<cs.UInt8>):Void;
	function Initialize():Void;
}
