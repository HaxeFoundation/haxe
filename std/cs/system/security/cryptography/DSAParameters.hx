package cs.system.security.cryptography;

/** Contains the typical parameters for the  algorithm. */
@:native("System.Security.Cryptography.DSAParameters")
extern class DSAParameters extends cs.system.ValueType {
	/** Specifies the counter for the  algorithm. */
	var Counter:Int;
	/** Specifies the  parameter for the  algorithm. */
	var G:cs.NativeArray<cs.UInt8>;
	/** Specifies the  parameter for the  algorithm. */
	var J:cs.NativeArray<cs.UInt8>;
	/** Specifies the  parameter for the  algorithm. */
	var P:cs.NativeArray<cs.UInt8>;
	/** Specifies the  parameter for the  algorithm. */
	var Q:cs.NativeArray<cs.UInt8>;
	/** Specifies the seed for the  algorithm. */
	var Seed:cs.NativeArray<cs.UInt8>;
	/** Specifies the  parameter for the  algorithm. */
	var X:cs.NativeArray<cs.UInt8>;
	/** Specifies the  parameter for the  algorithm. */
	var Y:cs.NativeArray<cs.UInt8>;
}
