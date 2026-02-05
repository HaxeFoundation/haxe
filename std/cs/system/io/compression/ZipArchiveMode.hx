package cs.system.io.compression;

/** Specifies values for interacting with zip archive entries. */
@:native("System.IO.Compression.ZipArchiveMode")
extern enum abstract ZipArchiveMode(Int) {
	var Create = 1;
	var Read = 0;
	var Update = 2;
}
