package flash.events;

@:require(flash10_1) extern class TouchEvent extends Event {
	var altKey : Bool;
	var ctrlKey : Bool;
	var isPrimaryTouchPoint : Bool;
	var isRelatedObjectInaccessible : Bool;
	var localX : Float;
	var localY : Float;
	var pressure : Float;
	var relatedObject : flash.display.InteractiveObject;
	var shiftKey : Bool;
	var sizeX : Float;
	var sizeY : Float;
	var stageX(default,null) : Float;
	var stageY(default,null) : Float;
	var touchPointID : Int;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, touchPointID : Int = 0, isPrimaryTouchPoint : Bool = false, localX : Float = 0./*NaN*/, localY : Float = 0./*NaN*/, sizeX : Float = 0./*NaN*/, sizeY : Float = 0./*NaN*/, pressure : Float = 0./*NaN*/, ?relatedObject : flash.display.InteractiveObject, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	function updateAfterEvent() : Void;
	static var TOUCH_BEGIN : String;
	static var TOUCH_END : String;
	static var TOUCH_MOVE : String;
	static var TOUCH_OUT : String;
	static var TOUCH_OVER : String;
	static var TOUCH_ROLL_OUT : String;
	static var TOUCH_ROLL_OVER : String;
	static var TOUCH_TAP : String;
}
