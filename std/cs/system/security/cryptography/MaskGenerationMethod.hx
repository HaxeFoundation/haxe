package cs.system.security.cryptography;

/** Represents the abstract class from which all mask generator algorithms must derive. */
@:native("System.Security.Cryptography.MaskGenerationMethod")
extern class MaskGenerationMethod {
	/**
	 * When overridden in a derived class, generates a mask with the specified length
	 * using the specified random seed.
	 * @param rgbSeed The random seed to use to compute the mask.
	 * @param cbReturn The length of the generated mask in bytes.
	 * @return A randomly generated mask whose length is equal to the  parameter.
	 */
	function GenerateMask(rgbSeed:cs.NativeArray<cs.UInt8>, cbReturn:Int):cs.NativeArray<cs.UInt8>;
}
