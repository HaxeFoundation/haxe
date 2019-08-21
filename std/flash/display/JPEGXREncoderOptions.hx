package flash.display;

extern final class JPEGXREncoderOptions {
	var colorSpace : BitmapEncodingColorSpace;
	var quantization : UInt;
	var trimFlexBits : UInt;
	function new(quantization : UInt = 20, ?colorSpace : BitmapEncodingColorSpace, trimFlexBits : UInt = 0) : Void;
}
