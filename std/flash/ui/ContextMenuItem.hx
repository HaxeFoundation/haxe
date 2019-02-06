package flash.ui;

extern final class ContextMenuItem extends flash.display.NativeMenuItem {
	var caption : String;
	var separatorBefore : Bool;
	var visible : Bool;
	function new(caption : String, separatorBefore : Bool = false, enabled : Bool = true, visible : Bool = true) : Void;
	function clone() : ContextMenuItem;
}
