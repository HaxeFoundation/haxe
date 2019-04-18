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
	var stageX(default,never) : Float;
	var stageY(default,never) : Float;
	var touchPointID : Int;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, touchPointID : Int = 0, isPrimaryTouchPoint : Bool = false, localX : Float = 0./*NaN*/, localY : Float = 0./*NaN*/, sizeX : Float = 0./*NaN*/, sizeY : Float = 0./*NaN*/, pressure : Float = 0./*NaN*/, ?relatedObject : flash.display.InteractiveObject, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	function updateAfterEvent() : Void;
	static final PROXIMITY_BEGIN : String;
	static final PROXIMITY_END : String;
	static final PROXIMITY_MOVE : String;
	static final PROXIMITY_OUT : String;
	static final PROXIMITY_OVER : String;
	static final PROXIMITY_ROLL_OUT : String;
	static final PROXIMITY_ROLL_OVER : String;
	static final TOUCH_BEGIN : String;
	static final TOUCH_END : String;
	static final TOUCH_MOVE : String;
	static final TOUCH_OUT : String;
	static final TOUCH_OVER : String;
	static final TOUCH_ROLL_OUT : String;
	static final TOUCH_ROLL_OVER : String;
	static final TOUCH_TAP : String;
}
