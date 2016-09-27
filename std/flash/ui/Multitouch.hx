package flash.ui;

@:require(flash10_1) extern class Multitouch {
	static var inputMode : MultitouchInputMode;
	static var mapTouchToMouse : Bool;
	static var maxTouchPoints(default,never) : Int;
	static var supportedGestures(default,never) : flash.Vector<String>;
	static var supportsGestureEvents(default,never) : Bool;
	static var supportsTouchEvents(default,never) : Bool;
}
