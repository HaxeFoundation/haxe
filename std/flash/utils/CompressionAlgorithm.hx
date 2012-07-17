package flash.utils;

@:fakeEnum(String) @:require(flash11) extern enum CompressionAlgorithm {
	DEFLATE;
	LZMA;
	ZLIB;
}
