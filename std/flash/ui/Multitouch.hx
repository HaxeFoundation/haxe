package flash.ui;

@:require(flash10_1) extern class Multitouch {
	static var inputMode : MultitouchInputMode;
	static var mapTouchToMouse : Bool;
	static var maxTouchPoints(default,null) : Int;
	static var supportedGestures(default,null) : flash.Vector<String>;
	static var supportsGestureEvents(default,null) : Bool;
	static var supportsTouchEvents(default,null) : Bool;
}
