package cs.system.security.cryptography;

/** Represents the standard parameters for the elliptic curve cryptography (ECC) algorithm. */
@:native("System.Security.Cryptography.ECParameters")
extern class ECParameters extends cs.system.ValueType {
	/** Represents the curve associated with the public key () and the optional private key (). */
	var Curve:cs.system.security.cryptography.ECCurve;
	/** Represents the private key  for the elliptic curve cryptography (ECC) algorithm, stored in big-endian format. */
	var D:cs.NativeArray<cs.UInt8>;
	/** Represents the public key  for the elliptic curve cryptography (ECC) algorithm. */
	var Q:cs.system.security.cryptography.ECPoint;
	/** Validates the current object. */
	function Validate():Void;
}
