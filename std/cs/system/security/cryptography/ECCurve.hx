package cs.system.security.cryptography;

/** Represents an elliptic curve. */
@:native("System.Security.Cryptography.ECCurve")
extern class ECCurve extends cs.system.ValueType {
	/** The first coefficient for an explicit curve. A for short Weierstrass, Montgomery, and Twisted Edwards curves. */
	var A:cs.NativeArray<cs.UInt8>;
	/** The second coefficient for an explicit curve. B for short Weierstrass and d for Twisted Edwards curves. */
	var B:cs.NativeArray<cs.UInt8>;
	/** The cofactor of the curve. */
	var Cofactor:cs.NativeArray<cs.UInt8>;
	/** Identifies the composition of the  object. */
	var CurveType:cs.system.security.cryptography.ECCurve_ECCurveType;
	/** The generator, or base point, for operations on the curve. */
	var G:cs.system.security.cryptography.ECPoint;
	/** The name of the hash algorithm which was used to generate the curve coefficients ( and ) from the  under the ANSI X9.62 generation algorithm. Applies only to explicit curves. */
	var Hash:Null<cs.system.security.cryptography.HashAlgorithmName>;
	/** The order of the curve. Applies only to explicit curves. */
	var Order:cs.NativeArray<cs.UInt8>;
	/** The curve polynomial. Applies only to characteristic 2 curves. */
	var Polynomial:cs.NativeArray<cs.UInt8>;
	/** The prime specifying the base field. Applies only to prime curves. */
	var Prime:cs.NativeArray<cs.UInt8>;
	/** The seed value for coefficient generation under the ANSI X9.62 generation algorithm. Applies only to explicit curves. */
	var Seed:cs.NativeArray<cs.UInt8>;
	/**
	 * Gets a value that indicates whether the curve type indicates an explicit
	 * characteristic 2 curve.
	 * @return if the curve is an explicit characteristic 2 curve;  if the curve is a
	 * named characteristic 2, prime, or implicit curve.
	 */
	var IsCharacteristic2(default, never):Bool;
	/**
	 * Gets a value that indicates whether the curve type indicates an explicit curve
	 * (either prime or characteristic 2).
	 * @return if the curve is an explicit curve (either prime or characteristic 2); 
	 * if the curve is a named or implicit curve.
	 */
	var IsExplicit(default, never):Bool;
	/**
	 * Gets a value that indicates whether the curve type indicates a named curve.
	 * @return if the curve is a named curve;  if the curve is an implicit or an 
	 * explicit curve (either prime or characteristic 2).
	 */
	var IsNamed(default, never):Bool;
	/**
	 * Gets a value that indicates whether the curve type indicates an explicit prime
	 * curve.
	 * @return if the curve is an explicit prime curve;  if the curve is a named prime,
	 * characteristic 2 or implicit curves.
	 */
	var IsPrime(default, never):Bool;
	/**
	 * Gets the identifier of a named curve.
	 * @return The identifier of a named curve.
	 */
	var Oid(default, never):cs.system.security.cryptography.Oid;
	/**
	 * Creates a named curve using the specified friendly name of the identifier.
	 * @param oidFriendlyName The friendly name of the identifier.
	 * @return An object representing the named curve.
	 */
	static function CreateFromFriendlyName(oidFriendlyName:String):cs.system.security.cryptography.ECCurve;
	/**
	 * Creates a named curve using the specified  object.
	 * @param curveOid The object identifier to use.
	 * @return An object representing the named curve.
	 */
	static function CreateFromOid(curveOid:cs.system.security.cryptography.Oid):cs.system.security.cryptography.ECCurve;
	/**
	 * Creates a named curve using the specified dotted-decimal representation of the
	 * identifier.
	 * @param oidValue The dotted number of the identifier.
	 * @return An object representing the named curve.
	 */
	static function CreateFromValue(oidValue:String):cs.system.security.cryptography.ECCurve;
	/** Validates the integrity of the current curve. Throws a  exception if the structure is not valid. */
	function Validate():Void;
}
