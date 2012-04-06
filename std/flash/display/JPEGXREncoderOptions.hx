package flash.display;

@:final extern class JPEGXREncoderOptions {
	var colorSpace : String;
	var quantization : UInt;
	var trimFlexBits : UInt;
	function new(quantization : UInt = 20, ?colorSpace : String, trimFlexBits : UInt = 0) : Void;
}
