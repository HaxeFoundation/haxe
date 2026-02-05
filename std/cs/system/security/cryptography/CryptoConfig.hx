package cs.system.security.cryptography;

/** Accesses the cryptography configuration information. */
@:native("System.Security.Cryptography.CryptoConfig")
extern class CryptoConfig {
	/**
	 * Indicates whether the runtime should enforce the policy to create only Federal
	 * Information Processing Standard (FIPS) certified algorithms.
	 * @return to enforce the policy; otherwise, .
	 */
	static var AllowOnlyFipsAlgorithms(default, never):Bool;
	function new():Void;
	/**
	 * Adds a set of names to algorithm mappings to be used for the current application
	 * domain.
	 * @param algorithm The algorithm to map to.
	 * @param names An array of names to map to the algorithm.
	 */
	static function AddAlgorithm(algorithm:cs.system.Type, names:cs.NativeArray<String>):Void;
	/**
	 * Adds a set of names to object identifier (OID) mappings to be used for the
	 * current application domain.
	 * @param oid The object identifier (OID) to map to.
	 * @param names An array of names to map to the OID.
	 */
	static function AddOID(oid:String, names:cs.NativeArray<String>):Void;
	@:overload(function(name:String):Dynamic {})
	/**
	 * Creates a new instance of the specified cryptographic object.
	 * @param name The simple name of the cryptographic object of which to create an
	 * instance.
	 * @return A new instance of the specified cryptographic object.
	 */
	static function CreateFromName(name:String, args:cs.NativeArray<Dynamic>):Dynamic;
	/**
	 * Encodes the specified object identifier (OID).
	 * @param str The OID to encode.
	 * @return A byte array containing the encoded OID.
	 */
	static function EncodeOID(str:String):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the object identifier (OID) of the algorithm corresponding to the specified
	 * simple name.
	 * @param name The simple name of the algorithm for which to get the OID.
	 * @return The OID of the specified algorithm.
	 */
	static function MapNameToOID(name:String):String;
}
