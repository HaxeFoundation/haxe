package flash.display;

extern class Stage extends DisplayObjectContainer {
	var align : StageAlign;
	var allowsFullScreen(default,never) : Bool;
	@:require(flash11_3) var allowsFullScreenInteractive(default,never) : Bool;
	var browserZoomFactor(default,never) : Float;
	@:require(flash10_2) var color : UInt;
	@:require(flash10) var colorCorrection : ColorCorrection;
	@:require(flash10) var colorCorrectionSupport(default,never) : ColorCorrectionSupport;
	@:require(flash11_4) var contentsScaleFactor(default,never) : Float;
	@:require(flash11) var displayContextInfo(default,never) : String;
	var displayState : StageDisplayState;
	var focus : InteractiveObject;
	var frameRate : Float;
	var fullScreenHeight(default,never) : UInt;
	var fullScreenSourceRect : flash.geom.Rectangle;
	var fullScreenWidth(default,never) : UInt;
	@:require(flash11_2) var mouseLock : Bool;
	var quality : StageQuality;
	var scaleMode : StageScaleMode;
	var showDefaultContextMenu : Bool;
	@:require(flash11) var softKeyboardRect(default,never) : flash.geom.Rectangle;
	@:require(flash11) var stage3Ds(default,never) : flash.Vector<Stage3D>;
	var stageFocusRect : Bool;
	var stageHeight : Int;
	@:require(flash10_2) var stageVideos(default,never) : flash.Vector<flash.media.StageVideo>;
	var stageWidth : Int;
	@:require(flash10_1) var wmodeGPU(default,never) : Bool;
	function invalidate() : Void;
	function isFocusInaccessible() : Bool;
}
