package cs.system.security.cryptography;

/** Computes the  hash for the input data. */
@:native("System.Security.Cryptography.SHA256")
extern class SHA256 extends cs.system.security.cryptography.HashAlgorithm {
	@:overload(function():cs.system.security.cryptography.SHA256 {})
	/**
	 * Creates an instance of the default implementation of .
	 * @return A new instance of . On the .NET Framework, this method creates an
	 * instance of the  class if FIPS mode is not active; if FIPS mode is active, it
	 * creates an instance of the  class. On .NET Core, it returns an instance of a
	 * private class derived from .
	 */
	static function Create(hashName:String):cs.system.security.cryptography.SHA256;
}
