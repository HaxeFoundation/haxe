package flash.display;

@:native("flash.display.ColorCorrectionSupport") @:require(flash10_1) extern enum abstract ColorCorrectionSupport(String) {
	var DEFAULT_OFF;
	var DEFAULT_ON;
	var UNSUPPORTED;
}
