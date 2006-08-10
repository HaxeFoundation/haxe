package flash.display;

extern class Stage extends flash.display.DisplayObjectContainer {
	function new() : Void;
	var align : String;
	var focus : flash.display.InteractiveObject;
	var frameRate : Float;
	function invalidate() : Void;
	function isFocusInaccessible() : Bool;
	var quality : String;
	var scaleMode : String;
	var showDefaultContextMenu : Bool;
	var stageFocusRect : Bool;
	var stageHeight : Int;
	var stageWidth : Int;
	private function requireOwnerPermissions() : Void;
}
