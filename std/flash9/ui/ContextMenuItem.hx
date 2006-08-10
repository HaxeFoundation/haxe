package flash.ui;

extern class ContextMenuItem extends flash.events.EventDispatcher {
	function new(caption : String, ?separatorBefore : Bool, ?enabled : Bool, ?visible : Bool) : Void;
	var caption : String;
	function clone() : flash.ui.ContextMenuItem;
	var enabled : Bool;
	var separatorBefore : Bool;
	var visible : Bool;
}
