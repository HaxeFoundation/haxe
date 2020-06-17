package flash.ui;

@:native("flash.ui.KeyLocation") extern enum abstract KeyLocation(UInt) from UInt to UInt {
	var ALPHANUMERIC;
	var KEYPAD;
	var NONE;
}
