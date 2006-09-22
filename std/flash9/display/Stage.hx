package flash.display;

extern class Stage extends DisplayObjectContainer {
	var align : StageAlign;
	var focus : InteractiveObject;
	var frameRate : Float;
	function invalidate() : Void;
	function isFocusInaccessible() : Bool;
	var quality : StageQuality;
	var scaleMode : StageScaleMode;
	var showDefaultContextMenu : Bool;
	var stageFocusRect : Bool;
	var stageHeight : Int;
	var stageWidth : Int;
	private function requireOwnerPermissions() : Void;
}
