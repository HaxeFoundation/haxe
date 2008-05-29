package flash.ui;

extern class ContextMenuClipboardItems {

	var cut : Bool;
	var paste : Bool;
	var copy : Bool;
	var selectAll : Bool;
	var clear : Bool;

	function new() : Void;
	function clone() : ContextMenuClipboardItems;
}

