package cs.system.security.cryptography;

/** Computes masks according to PKCS #1 for use by key exchange algorithms. */
@:native("System.Security.Cryptography.PKCS1MaskGenerationMethod")
extern class PKCS1MaskGenerationMethod extends cs.system.security.cryptography.MaskGenerationMethod {
	/**
	 * Gets or sets the name of the hash algorithm type to use for generating the mask.
	 * @return The name of the type that implements the hash algorithm to use for
	 * computing the mask.
	 */
	var HashName(default, default):String;
	function new():Void;
	/**
	 * Generates and returns a mask from the specified random seed of the specified
	 * length.
	 * @param rgbSeed The random seed to use for computing the mask.
	 * @param cbReturn The length of the generated mask in bytes.
	 * @return A randomly generated mask whose length is equal to the  parameter.
	 */
	function GenerateMask(rgbSeed:cs.NativeArray<cs.UInt8>, cbReturn:Int):cs.NativeArray<cs.UInt8>;
}
