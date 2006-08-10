package flash.events;

extern class MouseEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?localX : Float, ?localY : Float, ?relatedObject : flash.display.InteractiveObject, ?ctrlKey : Bool, ?altKey : Bool, ?shiftKey : Bool, ?buttonDown : Bool, ?delta : Int) : Void;
	var altKey : Bool;
	var buttonDown : Bool;
	var ctrlKey : Bool;
	var delta : Int;
	var localX : Float;
	var localY : Float;
	var relatedObject : flash.display.InteractiveObject;
	var shiftKey : Bool;
	var stageX(default,null) : Float;
	var stageY(default,null) : Float;
	function updateAfterEvent() : Void;
	private function getStageX(localX : Float, localY : Float) : Float;
	private function getStageY(localX : Float, localY : Float) : Float;
	private var m_altKey : Bool;
	private var m_buttonDown : Bool;
	private var m_ctrlKey : Bool;
	private var m_delta : Int;
	private var m_localX : Float;
	private var m_localY : Float;
	private var m_relatedObject : flash.display.InteractiveObject;
	private var m_shiftKey : Bool;
	static var CLICK : String;
	static var DOUBLE_CLICK : String;
	static var MOUSE_DOWN : String;
	static var MOUSE_MOVE : String;
	static var MOUSE_OUT : String;
	static var MOUSE_OVER : String;
	static var MOUSE_UP : String;
	static var MOUSE_WHEEL : String;
	static var ROLL_OUT : String;
	static var ROLL_OVER : String;
}
