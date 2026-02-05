package cs.system.io.compression;

/** Specifies values that indicate whether a compression operation emphasizes speed or compression size. */
@:native("System.IO.Compression.CompressionLevel")
extern enum CompressionLevel {
	Fastest;
	NoCompression;
	Optimal;
}
