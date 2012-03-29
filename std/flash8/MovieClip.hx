package flash;

private extern class MCBounds {
	var xMin : Float;
	var xMax : Float;
	var yMin : Float;
	var yMax : Float;
}

extern class MovieClip
#if !flash_strict
implements Dynamic
#end
{
	var useHandCursor : Bool;
	var enabled : Bool;
	var focusEnabled : Bool;
	//var menu : ContextMenu; rare, and we don't want to add __init__ for ContextMenu all the time
	var tabChildren : Bool;
	var tabEnabled : Bool;
	var tabIndex : Int;
	var hitArea : MovieClip;
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
	var _droptarget(default,null) : String;
	var _currentframe(default,null) : Int;
	var _totalframes(default,null) : Int;
	var _framesloaded(default,null) : Int;
	var _quality : String;
	var _focusrect : Bool;
	var _soundbuftime : Float;
	var _url : String;
	var _parent : MovieClip;

	function getURL( url : String, ?window : String, ?method : String ) : Void;
	function unloadMovie() : Void;
	function loadVariables( url : String, ?method : String ) : Void;
	function loadMovie( url : String, ?method : String ) : Void;
	function attachMovie(id : String, name : String, depth : Int, ?initObject : Dynamic) : MovieClip;

	#if flash_strict
	function swapDepths( depth : Int ) : Void;
	#else
	function swapDepths(mc : Dynamic) : Void;
	#end
	// function swapDepths( mc : String ) : Void;
	// function swapDepths( mc : MovieClip ) : Void;
	// function swapDepths( depth : Int ) : Void;

	function localToGlobal(pt : { x : Float, y : Float } ) : Void;
	function globalToLocal(pt : { x : Float, y : Float } ) : Void;

	// optional param problem if called with a second Bool parameter
	function hitTest( x_or_mc : Dynamic, ?y : Float, ?shape : Bool ) : Bool;

	// function hitTest( x : Float, y : Float, shape : Bool ) : Bool;
	// function hitTest( mc : MovieClip ) : Bool;
	function getBounds(bounds  :  MovieClip) : MCBounds;
	// don't allow function getBounds( bounds : String )
	function getBytesLoaded() : Int;
	function getBytesTotal() : Int;
	function attachAudio(id : Dynamic) : Void;
	function attachVideo(id : Dynamic) : Void;
	function getDepth() : Int;

	function setMask(mc : MovieClip) : Void;
	// don't allow setMask( mc : String ) : Void
	function play() : Void;
	function stop() : Void;
	function nextFrame() : Void;
	function prevFrame() : Void;
	function gotoAndPlay(frame : Dynamic) : Void;
	// frame : String | Int
	function gotoAndStop(frame : Dynamic) : Void;
	// frame : String | Int
	function duplicateMovieClip(name : String, depth : Int, ?initObject : Dynamic) : MovieClip;
	function removeMovieClip() : Void;
	function startDrag( lockCenter : Bool, ?left : Float, ?top : Float, ?right : Float, ?bottom : Float ) : Void;
	function stopDrag() : Void;
	function createEmptyMovieClip(name : String, depth : Int) : MovieClip;
	function beginFill(rgb : Int, ?alpha : Float) : Void;

#if !flash6
	function getInstanceAtDepth(depth : Int) : MovieClip;
	function getNextHighestDepth() : Int;
#end

#if flash8
	function beginGradientFill(fillType : String, colors : Array<Int>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, matrix : Dynamic, ?spreadMethod : String, ?interpolationMethod : String, ?focalPointRatio : Float ) : Void;
#else
	function beginGradientFill(fillType : String, colors : Array<Int>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, matrix : Dynamic) : Void;
#end
	// matrix : flash.geom.Matrix | Anonymous object a...i | Anonymous object (matrixType x,y,w,h,r)

	function moveTo(x : Float, y : Float) : Void;
	function lineTo(x : Float, y : Float) : Void;
	function curveTo(controlX : Float, controlY : Float, anchorX : Float, anchorY : Float) : Void;
#if flash8
	function lineStyle( ?thickness : Float, ?rgb : Int, ?alpha : Float, ?pixelHinting : Bool, ?noScale : String, ?capsStyle : String, ?jointStyle : String, ?miterLimit : Float) : Void;
#else
	function lineStyle( ?thickness : Float, ?rgb : Int, ?alpha : Float) : Void;
#end
	function endFill() : Void;
	function clear() : Void;

#if flash8
	function createTextField(instanceName : String, depth : Int, x : Float, y : Float, width : Float, height : Float) : TextField;
#else
	function createTextField(instanceName : String, depth : Int, x : Float, y : Float, width : Float, height : Float) : Void;
#end
	function getTextSnapshot() : TextSnapshot;
	function getSWFVersion() : Int;

	dynamic function onData() : Void;
	dynamic function onDragOut() : Void;
	dynamic function onDragOver() : Void;
	dynamic function onEnterFrame() : Void;
	dynamic function onKeyDown() : Void;
	dynamic function onKeyUp() : Void;
	dynamic function onKillFocus(newFocus : Dynamic) : Void;
	dynamic function onLoad() : Void;
	dynamic function onMouseDown() : Void;
	dynamic function onMouseMove() : Void;
	dynamic function onMouseUp() : Void;
	dynamic function onPress() : Void;
	dynamic function onRelease() : Void;
	dynamic function onReleaseOutside() : Void;
	dynamic function onRollOut() : Void;
	dynamic function onRollOver() : Void;
	dynamic function onSetFocus(oldFocus : Dynamic) : Void;
	dynamic function onUnload() : Void;

#if flash8
	var filters : Array<flash.filters.BitmapFilter>;
	var blendMode : Dynamic;
	var cacheAsBitmap : Bool;
	var opaqueBackground : Int;
	var scrollRect : Dynamic;
	var transform : flash.geom.Transform;
	var scale9Grid : flash.geom.Rectangle<Float>;

	function getRect( bounds : MovieClip ) : MCBounds;
	// don't allow bounds : String


	function attachBitmap( bmp : flash.display.BitmapData, depth : Int, ?pixelSnapping : String, ?smoothing : Bool ) : Void;
	function beginBitmapFill( bmp : flash.display.BitmapData , ?matrix:flash.geom.Matrix, ?repeat:Bool, ?smoothing:Bool ) : Void;
  	function lineGradientStyle( fillType:String, colors:Array<Int>, alphas:Array<Dynamic>, ratios:Array<Dynamic>, matrix:Dynamic, ?spreadMethod : String, ?interpolationMethod:String, ?focalPointRatio:Float ) : Void;

#end


	/** FP9 only **/
	var forceSmoothing : Bool;


// MT extension
#if flash_strict
	var smc : flash.MovieClip;
#end
	private static function __init__() : Void untyped {
		flash.MovieClip = _global["MovieClip"];
	}

}


