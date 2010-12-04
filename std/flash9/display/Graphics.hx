package flash.display;

@:final extern class Graphics {
	function new() : Void;
	function beginBitmapFill(bitmap : BitmapData, ?matrix : flash.geom.Matrix, repeat : Bool = true, smooth : Bool = false) : Void;
	function beginFill(color : UInt, alpha : Float = 1) : Void;
	function beginGradientFill(type : GradientType, colors : Array<UInt>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, ?matrix : flash.geom.Matrix, ?spreadMethod : SpreadMethod, ?interpolationMethod : InterpolationMethod, focalPointRatio : Float = 0) : Void;
	function clear() : Void;
	function curveTo(controlX : Float, controlY : Float, anchorX : Float, anchorY : Float) : Void;
	function drawCircle(x : Float, y : Float, radius : Float) : Void;
	function drawEllipse(x : Float, y : Float, width : Float, height : Float) : Void;
	function drawRect(x : Float, y : Float, width : Float, height : Float) : Void;
	function drawRoundRect(x : Float, y : Float, width : Float, height : Float, ellipseWidth : Float, ?ellipseHeight : Float) : Void;
	function drawRoundRectComplex(x : Float, y : Float, width : Float, height : Float, topLeftRadius : Float, topRightRadius : Float, bottomLeftRadius : Float, bottomRightRadius : Float) : Void;
	function endFill() : Void;
	function lineGradientStyle(type : GradientType, colors : Array<UInt>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, ?matrix : flash.geom.Matrix, ?spreadMethod : SpreadMethod, ?interpolationMethod : InterpolationMethod, focalPointRatio : Float = 0) : Void;
	function lineStyle(?thickness : Float, color : UInt = 0, alpha : Float = 1, pixelHinting : Bool = false, ?scaleMode : LineScaleMode, ?caps : CapsStyle, ?joints : JointStyle, miterLimit : Float = 3) : Void;
	function lineTo(x : Float, y : Float) : Void;
	function moveTo(x : Float, y : Float) : Void;
}
