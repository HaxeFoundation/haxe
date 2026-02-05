package cs.system.io.compression;

/** Specifies values for interacting with zip archive entries. */
@:native("System.IO.Compression.ZipArchiveMode")
extern enum ZipArchiveMode {
	Create;
	Read;
	Update;
}
