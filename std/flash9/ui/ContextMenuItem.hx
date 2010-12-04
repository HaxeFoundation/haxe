package flash.ui;

@:final extern class ContextMenuItem extends flash.events.EventDispatcher {
	var caption : String;
	var enabled : Bool;
	var separatorBefore : Bool;
	var visible : Bool;
	function new(caption : String, separatorBefore : Bool = false, enabled : Bool = true, visible : Bool = true) : Void;
	function clone() : ContextMenuItem;
}
