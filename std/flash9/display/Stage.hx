package flash.display;

extern class Stage extends DisplayObjectContainer {
	var align : StageAlign;
	@:require(flash10) var colorCorrection : ColorCorrection;
	@:require(flash10) var colorCorrectionSupport(default,null) : ColorCorrectionSupport;
	var displayState : StageDisplayState;
	var focus : InteractiveObject;
	var frameRate : Float;
	var fullScreenHeight(default,null) : UInt;
	var fullScreenSourceRect : flash.geom.Rectangle;
	var fullScreenWidth(default,null) : UInt;
	var quality : StageQuality;
	var scaleMode : StageScaleMode;
	var showDefaultContextMenu : Bool;
	var stageFocusRect : Bool;
	var stageHeight : Int;
	var stageWidth : Int;
	@:require(flash10_1) var wmodeGPU(default,null) : Bool;
	function invalidate() : Void;
	function isFocusInaccessible() : Bool;
}
