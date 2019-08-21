package flash.ui;

@:native("flash.ui.MultitouchInputMode") @:require(flash10_1) extern enum abstract MultitouchInputMode(String) {
	var GESTURE;
	var NONE;
	var TOUCH_POINT;
}
