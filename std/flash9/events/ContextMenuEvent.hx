package flash.events;

extern class ContextMenuEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?mouseTarget : flash.display.InteractiveObject, ?contextMenuOwner : flash.display.InteractiveObject) : Void;
	var contextMenuOwner : flash.display.InteractiveObject;
	var mouseTarget : flash.display.InteractiveObject;
	private var m_contextMenuOwner : flash.display.InteractiveObject;
	private var m_mouseTarget : flash.display.InteractiveObject;
	static var MENU_ITEM_SELECT : String;
	static var MENU_SELECT : String;
}
