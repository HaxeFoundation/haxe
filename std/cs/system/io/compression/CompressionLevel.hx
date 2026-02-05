package cs.system.io.compression;

/** Specifies values that indicate whether a compression operation emphasizes speed or compression size. */
@:native("System.IO.Compression.CompressionLevel")
extern enum abstract CompressionLevel(Int) {
	var Fastest = 1;
	var NoCompression = 2;
	var Optimal = 0;
}
