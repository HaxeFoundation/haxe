package cs.system.security.cryptography;

/** Represents the standard parameters for the  algorithm. */
@:native("System.Security.Cryptography.RSAParameters")
extern class RSAParameters extends cs.system.ValueType {
	/** Represents the  parameter for the  algorithm. */
	var D:cs.NativeArray<cs.UInt8>;
	/** Represents the  parameter for the  algorithm. */
	var DP:cs.NativeArray<cs.UInt8>;
	/** Represents the  parameter for the  algorithm. */
	var DQ:cs.NativeArray<cs.UInt8>;
	/** Represents the  parameter for the  algorithm. */
	var Exponent:cs.NativeArray<cs.UInt8>;
	/** Represents the  parameter for the  algorithm. */
	var InverseQ:cs.NativeArray<cs.UInt8>;
	/** Represents the  parameter for the  algorithm. */
	var Modulus:cs.NativeArray<cs.UInt8>;
	/** Represents the  parameter for the  algorithm. */
	var P:cs.NativeArray<cs.UInt8>;
	/** Represents the  parameter for the  algorithm. */
	var Q:cs.NativeArray<cs.UInt8>;
}
