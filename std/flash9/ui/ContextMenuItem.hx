package flash.ui;

extern class ContextMenuItem extends flash.events.EventDispatcher {
	var caption : String;
	var enabled : Bool;
	var separatorBefore : Bool;
	var visible : Bool;
	function new(caption : String, ?separatorBefore : Bool, ?enabled : Bool, ?visible : Bool) : Void;
	function clone() : ContextMenuItem;
}
