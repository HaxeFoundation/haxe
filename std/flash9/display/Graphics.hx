package flash.display;

extern class Graphics {
	function new() : Void;
	function beginBitmapFill(bitmap : flash.display.BitmapData, ?matrix : flash.geom.Matrix, ?repeat : Bool, ?smooth : Bool) : Void;
	function beginFill(color : UInt, ?alpha : Float) : Void;
	function beginGradientFill(type : String, colors : Array<Dynamic>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, ?matrix : flash.geom.Matrix, ?spreadMethod : String, ?interpolationMethod : String, ?focalPointRatio : Float) : Void;
	function clear() : Void;
	function curveTo(controlX : Float, controlY : Float, anchorX : Float, anchorY : Float) : Void;
	function drawCircle(x : Float, y : Float, radius : Float) : Void;
	function drawEllipse(x : Float, y : Float, width : Float, height : Float) : Void;
	function drawRect(x : Float, y : Float, width : Float, height : Float) : Void;
	function drawRoundRect(x : Float, y : Float, width : Float, height : Float, ellipseWidth : Float, ?ellipseHeight : Float) : Void;
	function drawRoundRectComplex(x : Float, y : Float, width : Float, height : Float, topLeftRadius : Float, topRightRadius : Float, bottomLeftRadius : Float, bottomRightRadius : Float) : Void;
	function endFill() : Void;
	function lineGradientStyle(type : String, colors : Array<Dynamic>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, ?matrix : flash.geom.Matrix, ?spreadMethod : String, ?interpolationMethod : String, ?focalPointRatio : Float) : Void;
	function lineStyle(?thickness : Float, ?color : UInt, ?alpha : Float, ?pixelHinting : Bool, ?scaleMode : String, ?caps : String, ?joints : String, ?miterLimit : Float) : Void;
	function lineTo(x : Float, y : Float) : Void;
	function moveTo(x : Float, y : Float) : Void;
}
