package flash.utils;

@:native("flash.utils.CompressionAlgorithm") @:require(flash11) extern enum abstract CompressionAlgorithm(String) {
	var DEFLATE;
	var LZMA;
	var ZLIB;
}
