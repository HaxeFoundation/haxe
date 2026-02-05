package cs.system.security.cryptography;

/** Represents a (X,Y) coordinate pair for elliptic curve cryptography (ECC) structures. */
@:native("System.Security.Cryptography.ECPoint")
extern class ECPoint extends cs.system.ValueType {
	/** Represents the X coordinate. */
	var X:cs.NativeArray<cs.UInt8>;
	/** Represents the Y coordinate. */
	var Y:cs.NativeArray<cs.UInt8>;
}
