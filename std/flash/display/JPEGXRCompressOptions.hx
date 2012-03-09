package flash.display;

@:final extern class JPEGXRCompressOptions implements IBitmapCompressOptions {
	var colorSpace : String;
	var quantization : UInt;
	var trimFlexBits : UInt;
	function new(quantization : UInt = 20, ?colorSpace : String, trimFlexBits : UInt = 0) : Void;
}
