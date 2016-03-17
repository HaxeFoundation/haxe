package flash.events;

extern class Event {
	var bubbles(default,null) : Bool;
	var cancelable(default,null) : Bool;
	var currentTarget(default,null) : Dynamic;
	var eventPhase(default,null) : EventPhase;
	var target(default,null) : Dynamic;
	var type(default,null) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false) : Void;
	function clone() : Event;
	function formatToString(className : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : String;
	function isDefaultPrevented() : Bool;
	function preventDefault() : Void;
	function stopImmediatePropagation() : Void;
	function stopPropagation() : Void;
	function toString() : String;
	static var ACTIVATE(default,never) : String;
	static var ADDED(default,never) : String;
	static var ADDED_TO_STAGE(default,never) : String;
	@:require(flash15) static var BROWSER_ZOOM_CHANGE(default,never) : String;
	static var CANCEL(default,never) : String;
	static var CHANGE(default,never) : String;
	static var CHANNEL_MESSAGE(default,never) : String;
	static var CHANNEL_STATE(default,never) : String;
	@:require(flash10) static var CLEAR(default,never) : String;
	static var CLOSE(default,never) : String;
	static var COMPLETE(default,never) : String;
	static var CONNECT(default,never) : String;
	@:require(flash11) static var CONTEXT3D_CREATE : String;
	@:require(flash10) static var COPY(default,never) : String;
	@:require(flash10) static var CUT(default,never) : String;
	static var DEACTIVATE(default,never) : String;
	static var ENTER_FRAME(default,never) : String;
	@:require(flash10) static var EXIT_FRAME(default,never) : String;
	@:require(flash10) static var FRAME_CONSTRUCTED(default,never) : String;
	@:require(flash11_3) static var FRAME_LABEL(default,never) : String;
	static var FULLSCREEN(default,never) : String;
	static var ID3 : String;
	static var INIT(default,never) : String;
	static var MOUSE_LEAVE(default,never) : String;
	static var OPEN(default,never) : String;
	@:require(flash10) static var PASTE(default,never) : String;
	static var REMOVED(default,never) : String;
	static var REMOVED_FROM_STAGE(default,never) : String;
	static var RENDER(default,never) : String;
	static var RESIZE(default,never) : String;
	static var SCROLL(default,never) : String;
	static var SELECT(default,never) : String;
	@:require(flash10) static var SELECT_ALL(default,never) : String;
	static var SOUND_COMPLETE(default,never) : String;
	@:require(flash11_3) static var SUSPEND(default,never) : String;
	static var TAB_CHILDREN_CHANGE(default,never) : String;
	static var TAB_ENABLED_CHANGE(default,never) : String;
	static var TAB_INDEX_CHANGE(default,never) : String;
	@:require(flash11_3) static var TEXTURE_READY(default,never) : String;
	@:require(flash11) static var TEXT_INTERACTION_MODE_CHANGE(default,never) : String;
	static var UNLOAD(default,never) : String;
	static var VIDEO_FRAME(default,never) : String;
	static var WORKER_STATE(default,never) : String;
}
