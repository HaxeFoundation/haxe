package cs.system.io.compression;

/** Specifies whether to compress or decompress the underlying stream. */
@:native("System.IO.Compression.CompressionMode")
extern enum abstract CompressionMode(Int) {
	var Compress = 1;
	var Decompress = 0;
}
