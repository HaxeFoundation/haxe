package flash.events;

extern class ContextMenuEvent extends Event {
	var contextMenuOwner : flash.display.InteractiveObject;
	var mouseTarget : flash.display.InteractiveObject;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?mouseTarget : flash.display.InteractiveObject, ?contextMenuOwner : flash.display.InteractiveObject) : Void;
	static var MENU_ITEM_SELECT : String;
	static var MENU_SELECT : String;
}
