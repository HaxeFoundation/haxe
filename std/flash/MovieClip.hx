package flash;

extern class MovieClip implements Dynamic
{
	var useHandCursor : Bool;
	var enabled : Bool;
	var focusEnabled : Bool;
	var tabChildren : Bool;
	var tabEnabled : Bool;
	var tabIndex : Float;
	var hitArea : Dynamic;
	var trackAsMenu : Bool;

	var _x : Float;
	var _y : Float;
	var _xmouse : Float;
	var _ymouse : Float;
	var _xscale : Float;
	var _yscale : Float;
	var _width : Float;
	var _height : Float;
	var _alpha : Float;
	var _lockroot : Bool;
	var _visible : Bool;
	var _target : String;
	var _rotation : Float;
	var _name : String;
	var _framesloaded : Float;
	var _droptarget : String;
	var _currentframe : Float;
	var _totalframes : Float;
	var _quality : String;
	var _focusrect : Bool;
	var _soundbuftime : Float;
	var _url : String;
	var _parent : MovieClip;

	function getURL(url : String, window : String, method : String) : Void;
	function unloadMovie() : Void;
	function loadVariables(url : String, method : String) : Void;
	function loadMovie(url : String, method : String) : Void;
	function attachMovie(id : String, name : String, depth : Float, initDynamic : Dynamic) : MovieClip;
	function swapDepths(mc : Dynamic) : Void;
	function localToGlobal(pt : Dynamic) : Void;
	function globalToLocal(pt : Dynamic) : Void;
	function hitTest(x : Dynamic, y : Dynamic) : Bool;
	function getBounds(bounds  :  Dynamic) : Dynamic;
	function getBytesLoaded() : Float;
	function getBytesTotal() : Float;
	function attachAudio(id : Dynamic) : Void;
	function attachVideo(id : Dynamic) : Void;
	function getDepth() : Float;
	function getInstanceAtDepth(depth : Float) : MovieClip;
	function getNextHighestDepth() : Float;
	function setMask(mc : Dynamic) : Void;
	function play() : Void;
	function stop() : Void;
	function nextFrame() : Void;
	function prevFrame() : Void;
	function gotoAndPlay(frame : Dynamic) : Void;
	function gotoAndStop(frame : Dynamic) : Void;
	function duplicateMovieClip(name : String, depth : Float, initDynamic : Dynamic) : MovieClip;
	function removeMovieClip() : Void;
	function startDrag(lockCenter : Bool, left : Float, top : Float, right : Float, bottom : Float) : Void;
	function stopDrag() : Void;
	function createEmptyMovieClip(name : String, depth : Float) : MovieClip;
	function beginFill(rgb : Float, alpha : Float) : Void;
	function beginGradientFill(fillType : String, colors : Array<Int>, alphas : Array<Float>, ratios : Array<Float>, matrix : Dynamic) : Void;
	function moveTo(x : Float, y : Float) : Void;
	function lineTo(x : Float, y : Float) : Void;
	function curveTo(controlX : Float, controlY : Float, anchorX : Float, anchorY : Float) : Void;
#if flash8
	function lineStyle(thickness : Float, rgb : Float, alpha : Float, pixelHinting : Bool, noScale : String, capsStyle : String, jointStyle : String, miterLimit : Float) : Void;
#else true
	function lineStyle(thickness : Float, rgb : Float, alpha : Float) : Void;
#end
	function endFill() : Void;
	function clear() : Void;

#if flash8
	function createTextField(instanceName : String, depth : Float, x : Float, y : Float, width : Float, height : Float) : TextField;
#else true
	function createTextField(instanceName : String, depth : Float, x : Float, y : Float, width : Float, height : Float) : Void;
#end
	function getTextSnapshot() : TextSnapshot;
	function getSWFVersion() : Float;

	function onData() : Void;
	function onDragOut() : Void;
	function onDragOver() : Void;
	function onEnterFrame() : Void;
	function onKeyDown() : Void;
	function onKeyUp() : Void;
	function onKillFocus(newFocus : Dynamic) : Void;
	function onLoad() : Void;
	function onMouseDown() : Void;
	function onMouseMove() : Void;
	function onMouseUp() : Void;
	function onPress() : Void;
	function onRelease() : Void;
	function onReleaseOutside() : Void;
	function onRollOut() : Void;
	function onRollOver() : Void;
	function onSetFocus(oldFocus : Dynamic) : Void;
	function onUnload() : Void;

#if flash8
	var filters : Array<flash.filters.BitmapFilter>;
	var blendMode : Dynamic;
	var cacheAsBitmap : Bool;
	var opaqueBackground : Float;
	var scrollRect : Dynamic;
	var transform : flash.geom.Transform;
	var scale9Grid : flash.geom.Rectangle<Float>;
	function getRect( bounds : Dynamic ) : Dynamic;
	function attachBitmap( bmp : flash.display.BitmapData, depth : Float, pixelSnapping : String, smoothing : Bool ) : Void;
#end

}


