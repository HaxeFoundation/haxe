package flash.events;

extern class Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool) : Void;
	var bubbles(default,null) : Bool;
	var cancelable(default,null) : Bool;
	function clone() : flash.events.Event;
	var currentTarget(default,null) : Dynamic;
	var eventPhase(default,null) : UInt;
	function formatToString(className : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : String;
	function isDefaultPrevented() : Bool;
	function preventDefault() : Void;
	function stopImmediatePropagation() : Void;
	function stopPropagation() : Void;
	var target(default,null) : Dynamic;
	function toString() : String;
	var type(default,null) : String;
	static var ACTIVATE : String;
	static var ADDED : String;
	static var CANCEL : String;
	static var CHANGE : String;
	static var CLOSE : String;
	static var COMPLETE : String;
	static var CONNECT : String;
	static var DEACTIVATE : String;
	static var ENTER_FRAME : String;
	static var ID3 : String;
	static var INIT : String;
	static var MOUSE_LEAVE : String;
	static var OPEN : String;
	static var REMOVED : String;
	static var RENDER : String;
	static var RESIZE : String;
	static var SCROLL : String;
	static var SELECT : String;
	static var SOUND_COMPLETE : String;
	static var TAB_CHILDREN_CHANGE : String;
	static var TAB_ENABLED_CHANGE : String;
	static var TAB_INDEX_CHANGE : String;
	static var UNLOAD : String;

	/** added in FP 9.0.28 **/
	static var ADDED_TO_STAGE : String;

	/** added in FP 9.0.28 **/
	static var REMOVED_FROM_STAGE : String;

	/** added in FP 9.0.28 **/
	static var FULLSCREEN : String;

}
