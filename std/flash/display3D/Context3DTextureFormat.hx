package flash.display3D;

@:native("flash.display3D.Context3DTextureFormat") extern enum abstract Context3DTextureFormat(String) {
	var BGRA;
	var BGRA_PACKED;
	var BGR_PACKED;
	var COMPRESSED;
	var COMPRESSED_ALPHA;
	var RGBA_HALF_FLOAT;
}
